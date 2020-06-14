{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module SimplIR.LearningToRankWrapper
    ( Model(..)
    , SomeModel(..)
    , WeightVec(..)
    , modelWeights
    , modelFeatures
    , toFeatures'
    , toDocFeatures'
    , totalRelevantFromQRels
    , totalRelevantFromData
    , augmentWithQrels
    , learnToRank
    , rerankRankings
    , rerankRankings'
    , untilConverged
    , defaultConvergence, relChangeBelow, dropIterations, maxIterations, ConvergenceCriterion

    ) where

import GHC.Generics
import Data.Coerce

import Control.DeepSeq
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.HashMap.Strict as HM
import Data.Foldable (toList)
import Data.Maybe
import System.Random
import Debug.Trace
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Aeson.Types as Aeson
import Data.Aeson as Aeson


import SimplIR.LearningToRank
import SimplIR.FeatureSpace (FeatureSpace, FeatureVec)
import qualified SimplIR.FeatureSpace as FS
import qualified SimplIR.Format.TrecRunFile as Run
import qualified SimplIR.Format.QRel as QRel

type ConvergenceCriterion f s = ([(Double, WeightVec f s)] -> [(Double, WeightVec f s)])

data SomeModel f where
    SomeModel :: forall f s. Model f s -> SomeModel f

newtype Model f s = Model { modelWeights' :: WeightVec f s }
                  deriving (Show, Generic)
instance NFData (Model f s) where rnf = (`seq` ())

modelWeights :: Model f s -> [(f, Double)]
modelWeights (Model weights) = FS.toList (getWeightVec weights)

modelFeatures :: Model f s -> FeatureSpace f s
modelFeatures = FS.featureSpace . getWeightVec . modelWeights'




-- ---------- JSON Serialization -------


newtype Assocs k v = Assocs { getAssocs :: [(k,v)] }

instance (Aeson.ToJSONKey k, Aeson.ToJSON v) => Aeson.ToJSON (Assocs k v) where
    toJSON =
        case Aeson.toJSONKey of
            Aeson.ToJSONKeyText f _ -> 
                let toPair (k,v) = f k Aeson..= Aeson.toJSON v
                in Aeson.object . map toPair . getAssocs
            Aeson.ToJSONKeyValue f _ -> 
                Aeson.toJSON . map (\(k,v) -> (f k, Aeson.toJSON v)) . getAssocs
    toEncoding =
        case Aeson.toJSONKey of
            Aeson.ToJSONKeyText f _ -> 
                let toPair (k,v) = f k Aeson..= Aeson.toJSON v
                in Aeson.pairs . foldMap toPair . getAssocs
            Aeson.ToJSONKeyValue f _ -> 
                Aeson.toEncoding . map (\(k,v) -> (f k, Aeson.toJSON v)) . getAssocs
    
instance (Aeson.FromJSONKey k, Aeson.FromJSON v) => Aeson.FromJSON (Assocs k v) where
    parseJSON v =
        case Aeson.fromJSONKey of
            Aeson.FromJSONKeyValue f -> 
                flip (Aeson.withArray "association list") v $ (fmap Assocs . mapM (fromPair f) . Data.Foldable.toList)
            Aeson.FromJSONKeyText f ->
                flip (Aeson.withObject "association list") v $ (fmap Assocs . mapM (fromPair (\v -> Aeson.withText "key" (pure . f) v)) . Data.Foldable.toList)
            Aeson.FromJSONKeyTextParser f ->
                flip (Aeson.withObject "association list") v $ (fmap Assocs . mapM (fromPair (\v -> Aeson.withText "key" f v)) . Data.Foldable.toList)
            Aeson.FromJSONKeyCoerce _ ->
                flip (Aeson.withObject "association list") v $ (fmap Assocs . mapM (fromPair (pure . unsafeCoerce)) . Data.Foldable.toList)
      where
        fromPair :: (Aeson.Value -> Aeson.Parser k) -> Aeson.Value -> Aeson.Parser (k, v)
        fromPair f pair = do
            (k,v) <- Aeson.parseJSON pair
            (,) <$> f k <*> Aeson.parseJSON v

-- TODO also save feature space to ensure features idx don't change semantics
instance (Ord f, Aeson.ToJSONKey f) => Aeson.ToJSON (Model f s) where
    toJSON (Model weights) =
        Aeson.toJSON $ Assocs $ FS.toList (getWeightVec weights)
    toEncoding (Model weights) =
        Aeson.toEncoding $ Assocs $ FS.toList (getWeightVec weights)

instance (Ord f, Show f, Aeson.FromJSONKey f) => Aeson.FromJSON (SomeModel f) where
    parseJSON = fmap toSomeModel . Aeson.parseJSON
      where
        toSomeModel :: Assocs f Double -> SomeModel f
        toSomeModel (Assocs pairs) = 
            let FS.SomeFeatureSpace fspace = FS.mkFeatureSpace $ S.fromList $ map fst pairs
                weights = WeightVec $ FS.fromList fspace pairs
            in SomeModel $ Model weights

instance (Ord f, Aeson.ToJSONKey f) => Aeson.ToJSON (SomeModel f) where
    toJSON (SomeModel model) = 
        Aeson.toJSON $ Assocs $ FS.toList $ getWeightVec $ modelWeights' model





-- -------------------------------------




runToDocFeatures :: Ord f
                 => M.Map f [Run.RankingEntry]
                 -> M.Map (Run.QueryId, QRel.DocumentName) (M.Map f Double)
runToDocFeatures runFiles = M.fromListWith M.union
            [ ( (Run.queryId entry, Run.documentName entry)
              , M.singleton featureName (Run.documentScore entry) )
            | (featureName, ranking) <- M.toList runFiles
            , entry <- ranking
            ]


toDocFeatures' :: (Show f, Ord f)
               => FeatureSpace f s
               -> M.Map f [Run.RankingEntry]
               -> M.Map (Run.QueryId, QRel.DocumentName) (FeatureVec f s Double)
toDocFeatures' fspace runFiles =
    fmap (toFeatures' fspace) (runToDocFeatures runFiles)

toFeatures' :: (Show f, Ord f) => FeatureSpace f s -> M.Map f Double -> FeatureVec f s Double
toFeatures' fspace features =
    FS.fromList fspace [ (f, features M.! f) | f <- FS.featureNames fspace ]

-- | Compute a function for the total number of relevant documents for a set of
-- queries.
totalRelevantFromQRels :: Ord query
              => [QRel.Entry query doc IsRelevant]
              -> query -> TotalRel
totalRelevantFromQRels qrel =
    fromMaybe 0 . (`M.lookup` totalRel)
  where
    totalRel =
        M.fromListWith (+)
        [ (qid, n)
        | QRel.Entry qid _ rel <- qrel
        , let n = case rel of Relevant -> 1
                              NotRelevant -> 0
        ]

totalRelevantFromData
    :: forall query doc f s. (Ord query)
    => M.Map query [(doc, FeatureVec f s Double, IsRelevant)]
    -> query -> TotalRel
totalRelevantFromData trainData =
    fromMaybe 0 . (`M.lookup` totalRel)
  where
    totalRel = fmap (length . filter (\(_,_, rel)-> (rel == Relevant))) trainData

augmentWithQrels :: forall docId queryId f s.
                    (Ord queryId, Ord docId)
                 => [QRel.Entry queryId docId IsRelevant]
                 -> M.Map (queryId, docId) (FeatureVec f s Double)
                 -> M.Map queryId [(docId, FeatureVec f s Double, IsRelevant)]
augmentWithQrels qrel docFeatures =
    let relevance :: M.Map (queryId, docId) IsRelevant
        relevance = M.fromList [ ((qid, doc), rel)
                               | QRel.Entry qid doc rel <- qrel
                               ]

        franking :: M.Map queryId [(docId, FeatureVec f s Double, IsRelevant)]
        franking = M.fromListWith (++)
                   [ (qid, [(doc, features, relDocs)])
                   | ((qid, doc), features) <- M.assocs docFeatures
                   , let relDocs = M.findWithDefault NotRelevant (qid, doc) relevance
                   ]
    in franking



learnToRank :: forall f s query docId. (Ord query, Show query, Show docId, Show f)
            => MiniBatchParams
            -> ConvergenceCriterion f s
            -> EvalCutoff
            -> M.Map query [(docId, FeatureVec f s Double, IsRelevant)]
            -> FeatureSpace f s
            -> ScoringMetric IsRelevant query
            -> StdGen
            -> (Model f s, Double)
learnToRank miniBatchParams convergence evalCutoff franking fspace metric gen0 =
    let weights0 :: WeightVec f s
        weights0 = WeightVec $ FS.repeat fspace 1
        iters =
            let optimise gen w trainData =
                    map snd $ coordAscent evalCutoff metric gen w trainData
            in miniBatchedAndEvaluated miniBatchParams
                 metric optimise gen0 weights0 franking
        errorDiag = show weights0 ++ ". Size training queries: "++ show (M.size franking)++ "."
        checkNans (_,_) (b,_)
           | isNaN b = error $ "Metric score is NaN. initial weights " ++ errorDiag
           | otherwise = True
        checkedConvergence :: [(Double, WeightVec f s)] -> [(Double, WeightVec f s)]
        checkedConvergence = untilConverged checkNans . convergence
        (evalScore, weights) = case checkedConvergence iters of
           []          -> error $ "learning converged immediately. "++errorDiag
           itersResult -> last itersResult
    in (Model weights, evalScore)

traceIters :: String -> [(Double, a)] -> [(Double, a)]
traceIters info = go (1 :: Int)
  where
    go _ [] = []
    go i (x0:x1:xs) =
        trace msg (x0 : go (i+1) (x1:xs))
      where
        msg = concat [ info, " iteration ", show i
                     , ", score ", show s0, " -> ", show s1
                     , " rel ", show (relChange s0 s1)
                     ]
        s0 = fst x0
        s1 = fst x1

defaultConvergence :: String -> Double -> Int -> Int -> [(Double, WeightVec f s)] -> [(Double, WeightVec f s)]
defaultConvergence info threshold maxIter dropIter =
    relChangeBelow threshold . maxIterations maxIter . dropIterations dropIter . traceIters info



relChange :: RealFrac a => a -> a -> a
relChange a b = abs (a-b) / abs b

rerankRankings :: Model f s
               -> M.Map Run.QueryId [(QRel.DocumentName, FeatureVec f s Double)]
               -> M.Map Run.QueryId (Ranking Score QRel.DocumentName)
rerankRankings model featureData  =
    fmap (rerank (modelWeights' model)) featureData

rerankRankings' :: Model f s
                -> M.Map q [(docId, FeatureVec f s Double, rel)]
                -> M.Map q (Ranking Score (docId, rel))
rerankRankings' model featureData  =
    fmap (rerank (modelWeights' model) . rearrangeTuples) featureData
  where rearrangeTuples = (fmap (\(d,f,r)-> ((d,r), f)))

untilConverged :: (a -> a -> Bool) -> [a] -> [a]
untilConverged conv xs0 = go xs0
  where
    go (x:y:_)
      | x `conv` y  = [x, y]
    go (x:rest)     = x : go rest
    go []           = []

maxIterations :: Int -> [a] -> [a]
maxIterations = take
dropIterations :: Int -> [a] -> [a]
dropIterations = drop

relChangeBelow :: Double -> [(Double, b)] -> [(Double, b)]
relChangeBelow threshold xs =
    untilConverged (\(x,_) (y,_) -> relChange x y < threshold) xs
