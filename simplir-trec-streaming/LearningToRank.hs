{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad (join, when)
import Data.Foldable
import Data.Monoid
import Data.Maybe

import qualified Data.Aeson.Types as Aeson
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Yaml as Yaml

import System.Random

import qualified Data.SmallUtf8 as Utf8
import Query
import SimplIR.LearningToRank
import Types
import SimplIR.Types
import Utils

import Options.Applicative

{-# ANN module ("HLint: ignore Redundant lambda"::String) #-}

extractFeaturesFromDocument :: (FeatureName -> Double)
                            -> V.Vector FeatureName -> ScoredDocument
                            -> Features
extractFeaturesFromDocument defaultFeature features doc =
    let lookupFeature :: FeatureName -> Double
        lookupFeature fn =
            maybe def fromNumber $ M.lookup rvn (scoredRecordedValues doc)
          where
            def = defaultFeature fn

            rvn :: RecordedValueName
            rvn = recordedFeatureName fn

            fromNumber :: Aeson.Value -> Double
            fromNumber = fromResult . Aeson.fromJSON
              where
                fromResult (Aeson.Error msg) =
                    error $ "Error looking up value for feature "++show fn++": "++msg
                fromResult (Aeson.Success x) = x
    in Features $ VU.convert $ fmap lookupFeature features

readQRel :: FilePath -> IO (M.Map QueryId (M.Map DocumentName IsRelevant))
readQRel fname =
    toMap . mapMaybe parseLine . lines <$> readFile fname
  where
    toMap :: [(QueryId, DocumentName, IsRelevant)]
          -> M.Map QueryId (M.Map DocumentName IsRelevant)
    toMap xs =
      M.unionsWith M.union [ M.singleton qid (M.singleton docName isRel)
                           | (qid, docName, isRel) <- xs ]

    parseLine :: String -> Maybe (QueryId, DocumentName, IsRelevant)
    parseLine line =
      case words line of
        [queryId, _dump, docId, relevance] ->
          let rel = case relevance of "0" -> NotRelevant
                                      _   -> Relevant
              !qid = QueryId $ T.pack queryId
              !docName = DocName $ Utf8.fromString docId
          in Just (qid, docName, rel)

        _ -> Nothing

options :: Parser (IO ())
options =
    train
      <$> option str (short 'q' <> long "queries" <> metavar "FILE" <> help "A queries file")
      <*> option str (short 'Q' <> long "qrels" <> metavar "FILE" <> help "A qrel file")
      <*> option str (short 'f' <> long "features" <> metavar "FILE" <> help "A feature file")

main :: IO ()
main = join $ execParser $ info (helper <*> options) mempty

train :: FilePath -> FilePath -> FilePath -> IO ()
train queriesPath qrelPath resultsPath = do
    qs <- readQueries queriesPath
    let featureNames :: V.Vector FeatureName
        featureNames = V.fromList $ S.toList $ foldMap queryFeatures qs

    Just (Results allScoredDocs) <- Yaml.decodeFile resultsPath :: IO (Maybe Results)

    -- TODO: handle multiple parameter settings
    let paramSettingNames = S.fromList $ map snd $ M.keys allScoredDocs
    paramSettingName <-
      case S.toList paramSettingNames of
        [name] -> return name
        _      -> fail "Expected only one parameter setting"

    let scoredDocs :: M.Map QueryId [ScoredDocument]
        scoredDocs =
            M.mapKeys fst
            $ M.filterWithKey (\(_,psn) _ -> psn == paramSettingName) allScoredDocs

    qrels <- readQRel qrelPath
    let fRankings :: M.Map QueryId (FRanking IsRelevant DocumentName)
        fRankings = M.unionsWith (++)
          [ M.singleton qid [(name, featureVec, isRelevant)]
          | (qid, sds) <- M.toList scoredDocs
          , sd <- sds
          , let name = docName $ scoredDocumentInfo sd
          , let isRelevant = fromMaybe NotRelevant
                             $ M.lookup qid qrels >>= M.lookup name
          , let featureVec = extractFeaturesFromDocument defaultFeature featureNames sd
          ]
        defaultFeature = const (-1000)


    let totalRelInFRanking :: FRanking IsRelevant DocumentName -> TotalRel
        totalRelInFRanking = length . filter (\(_, _, rel) -> rel == Relevant)

        totalRel :: M.Map QueryId TotalRel
        totalRel = fmap totalRelInFRanking fRankings

        totalRel' :: QueryId -> TotalRel
        totalRel' qid = M.findWithDefault 0 qid totalRel

    when (all (== 0) totalRel) $ fail "Error: No relevant documents"
    gen <- newStdGen

    let initWeights :: Features
        initWeights = Features $ VU.convert $ V.map (const 1) featureNames

        untilConverged :: (a -> a -> Bool) ->  [a] -> [a]
        untilConverged eq xs = map snd $ takeWhile (\(a,b) -> not $ a `eq` b) $ zip xs (tail xs)

        iterates = untilConverged (\(a,_) (b,_) -> abs (a-b) < 1e-8)
                   $ coordAscent gen (meanAvgPrec totalRel' Relevant) initWeights fRankings
        (evalScore, weights) = last iterates

    mapM_ print $ take 100 iterates
    print weights
    print evalScore


    let perFeatureScores :: M.Map FeatureName Score
        perFeatureScores =
            fmap (\w -> meanAvgPrec totalRel' Relevant $ fmap (w `weightRanking`) fRankings) oneVectors

        oneVectors :: M.Map FeatureName Features
        oneVectors = fold
            [ M.singleton featName ( Features $ VU.convert
                                   $ V.imap (\i' _ -> if i == i' then 1 else 0) featureNames
                                   )
            | (i, featName) <- toList $ V.indexed featureNames
            ]

    print perFeatureScores
