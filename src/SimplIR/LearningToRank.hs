{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module LearningToRank where

import Data.Ord
import Data.List
import Data.Bifunctor
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as V

type Score = Double
data IsRelevant = NotRel | Relevant
                deriving (Ord, Eq, Show)

-- | A ranking of documents along with relevant annotations
type Ranking relevance a = [(a, Score, relevance)]

-- | The total number of relevant documents for a query.
type TotalRel = Int

-- | A collection of rankings for a set of queries.
type Rankings rel qid a = M.Map qid (Ranking rel a, TotalRel)

meanAvgPrec :: (Ord rel)
            => rel -> Rankings rel qid a -> Double
meanAvgPrec relThresh rankings =
    mean (fmap (avgPrec relThresh) (M.elems rankings))

mean :: (RealFrac a) => [a] -> a
mean xs = sum xs / realToFrac (length xs)

avgPrec :: forall rel a. (Ord rel)
        => rel -> (Ranking rel a, TotalRel) -> Double
avgPrec relThresh (ranking, totalRel) =
    let (_, relAtR) = mapAccumL numRelevantAt 0 rels
        rels = map (\(_, _, rel) -> rel) ranking

        numRelevantAt :: Int -> rel -> (Int, Int)
        numRelevantAt accum rel
          | rel >= relThresh = (accum + 1, accum + 1)
          | otherwise        = (accum, accum)

        precAtR :: [(Double, rel)]
        precAtR = zip (zipWith (\n k -> realToFrac n / k) relAtR [1..]) rels

        precAtRelevantRanks = [ prec
                              | (prec, rel) <- precAtR
                              , rel >= relThresh
                              ]
    in
        sum precAtRelevantRanks / realToFrac totalRel


newtype Features = Features (V.Vector Double)
                 deriving (Show)

featureDim :: Features -> Int
featureDim (Features v) = V.length v

stepFeature :: Step -> Features -> Features
stepFeature (Step dim delta) (Features v) =
    Features $ V.update v (V.fromList [(dim, v V.! dim + delta)])

-- | A ranking of documents along with relevant annotations
type FRanking relevance a = [(a, Features, relevance)]

dot :: Features -> Features -> Double
dot (Features v) (Features u) = V.sum $ V.zipWith (*) v u

data Step = Step Int Double

zeroStep :: Step
zeroStep = Step 0 0

isZeroStep :: Step -> Bool
isZeroStep (Step _ d) = d == 0

-- | @dotStepOracle w f delta == (w + delta) `dot` f@.
dotStepOracle :: Features -> Features -> Step -> Double
dotStepOracle w@(Features w') f@(Features f') = get
  where
    get s | isZeroStep s = dot0
    get (Step dim step)  = dot0 - term dim 0 + term dim step

    term dim off = (off + w' V.! dim) * (f' V.! dim)
    !dot0 = w `dot` f

coordAscent :: forall a qid relevance. (Ord relevance)
            => relevance
            -> Features -- ^ initial weights
            -> M.Map qid (FRanking relevance a, TotalRel)
            -> [(Score, Features)]
coordAscent relThresh w0 fRankings = iterate go (0, w0)
  where
    dim = featureDim w0
    steps = [ f x
            | x <- [10, 1, 0.1, 0.01, 0.001]
            , f <- [id, negate]
            ]
    go :: (Score, Features) -> (Score, Features)
    go w =
      foldl' updateDim w [0..dim-1]

    updateDim :: (Score, Features) -> Int -> (Score, Features)
    updateDim (_, w) dim =
        maximumBy (comparing fst)
        [ (scoreStep step, stepFeature step w)
        | delta <- steps
        , let step = Step dim delta
        ]
      where
        scoreStep :: Step -> Score
        scoreStep step =
            meanAvgPrec relThresh
            $ fmap (first $ sortDocs . map (\(doc,f,rel) -> (doc, f step, rel)))
            $ cachedFeatures
          where
            sortDocs = sortBy (comparing $ \(_,loss,_) -> loss)

        cachedFeatures :: M.Map qid ([(a, Step -> Score, relevance)], TotalRel)
        cachedFeatures =
            (fmap . first) (sortDocs . map (\(doc,f,rel) -> (doc, dotStepOracle w f, rel))) $ fRankings
          where
            sortDocs = sortBy (comparing $ \(_,f,_) -> f zeroStep)


-----------------------
-- Testing

type DocId = String
type TestRanking = Ranking IsRelevant DocId

rankingA :: TestRanking
rankingA =
    [ ("ben", 10, Relevant)
    , ("laura", 9, Relevant)
    , ("T!", 3, Relevant)
    , ("snowman", 4, NotRel)
    , ("cat", 5, NotRel)
    ]

rankingB :: TestRanking
rankingB =
    [ ("ben", 3, NotRel)
    , ("laura", 9, Relevant)
    , ("snowman", 4, Relevant)
    ]

rankingC :: TestRanking
rankingC =
    [ ("cat", 9, Relevant)
    , ("T!", 10, Relevant)
    ]

testFeatures :: M.Map Char (FRanking IsRelevant DocId, Int)
testFeatures =
    fmap (first $ map toFeatures) testRankings
  where
    toFeatures (a,b,c) = (a, Features $ V.fromList [1,b], c)

testRankings :: M.Map Char (TestRanking, Int)
testRankings =
    M.fromList $ zip ['a'..] $ zip [rankingA, rankingB, rankingC] (repeat 10)