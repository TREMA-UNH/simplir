{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module SimplIR.Ranking.Evaluation
    (-- * Scoring Metric Types
    ScoringMetric, ApproxScoringMetric, GenericScoringMetric
    , AbstractScoringMetric
    -- * MAP
    , TotalRel
    , meanAvgPrec
    , avgPrec
    , naiveAvgPrec
    -- * Approx MAP
    , meanAvgPrecApprox
    -- * convert Ranking data structure
    , ProduceRankingsForEval
    , produceRankingsForEval, produceHashedRankingsForEval
    ) where

import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG

import SimplIR.Ranking (Ranking)
import SimplIR.Types.Relevance
import qualified SimplIR.Ranking as Ranking
import qualified SimplIR.HashedRanking as HR



-- | An abstract scoring method for a taking a set of queries with some ranking data type r to produce an evaluation score.
type AbstractScoringMetric r rel qid = forall a. M.Map qid (r Double (a,rel)) -> Double

-- | A scoring method, taking a set of queries and their rankings to a score.
type ScoringMetric rel qid = AbstractScoringMetric Ranking rel qid
-- type ScoringMetric rel qid = forall a. M.Map qid (Ranking Double (a,rel)) -> Double


-- | A generic scoring method, taking a set of queries and their rankings to a score.
type GenericScoringMetric rel qid = forall a. M.Map qid ([(Double, (a,rel))]) -> Double


-- | A scoring method, taking a set of queries and their rankings to a score.
type ApproxScoringMetric rel qid = AbstractScoringMetric HR.HashedRanking rel qid
-- type ApproxScoringMetric rel qid = forall a. M.Map qid (HR.HashedRanking Double (a,rel)) -> Double


type ProduceRankingsForEval r a rel = ([(Double, (a,rel))] -> r Double (a,rel))

produceRankingsForEval :: ProduceRankingsForEval Ranking a rel
produceRankingsForEval xs = Ranking.fromList xs


produceHashedRankingsForEval :: ProduceRankingsForEval HR.HashedRanking a rel
produceHashedRankingsForEval xs = HR.fromList xs




-- | The total number of relevant documents for a query.
type TotalRel = Int




meanAvgPrecApprox :: forall rel qid . (Ord rel)
            => (qid -> TotalRel) -> rel -> ApproxScoringMetric rel qid
meanAvgPrecApprox totalRel relThresh rankings 
  | null rankings = error "meanAvgPrecApprox with no rankings"
  | otherwise  = 
      mean $ map perQuery $ M.toList $ rankings
  where
    perQuery (qid, ranking) =
        fromMaybe 0 $ avgPrecApprox relThresh (totalRel qid) ranking


avgPrecApprox :: forall rel doc score. (Ord rel, VU.Unbox score, Real score)
        => rel       -- ^ threshold of relevance (inclusive)
        -> TotalRel  -- ^ total number of relevant documents
        -> HR.HashedRanking score (doc, rel)  -- ^ ranking
        -> Maybe Double  -- ^ 'Nothing' if no relevant documents
avgPrecApprox relThresh totalRel ranking
  | totalRel == 0 = Nothing
  | otherwise =
    let counts :: [(Int,Int)]
        counts = fmap snd $ HR.bucketCounts  (\(_, rel) -> rel >= relThresh) ranking

        -- Implementing the following approximation:
        -- sum_{k-1}^K  (  R_k  \frac{\sum_{l<=k} R_l }{ \sum_{l<=k} N_l })
         
        cumsums :: [BucketCounts] 
        cumsums = filter (\(BucketCounts _ n'  _  cn') ->  n'>0 && cn'>0)
                $  scanl' accum emptyBucketCounts $  counts
        unnormMap = 
          sum [ r' * cr'/cn' 
            | BucketCounts r n cr cn <-  cumsums  -- ints
            , let (r', n', cr', cn') = (realToFrac r, realToFrac n, realToFrac cr, realToFrac cn) -- reals
            ]
    in Just $ unnormMap / realToFrac totalRel    
  where accum :: BucketCounts -> (Int, Int) -> BucketCounts
        accum (BucketCounts _ _ cr cn) (r, n) =
            BucketCounts r n (cr+r)  (cn + n)

data BucketCounts = BucketCounts {
        rels :: !Int
        , counts :: !Int
        , cumRels :: !Int
        , cumCounts :: !Int 
}
emptyBucketCounts :: BucketCounts
emptyBucketCounts =BucketCounts 0 0 0 0

{-# SPECIALISE
    avgPrecApprox :: forall rel doc. (Ord rel)
            => rel
            -> TotalRel
            -> HR.HashedRanking Double (doc, rel)
            -> Maybe Double
    #-}
{-# SPECIALISE
    avgPrecApprox :: IsRelevant
            -> TotalRel
            -> HR.HashedRanking Double (doc, IsRelevant)
            -> Maybe Double
    #-}

-- ==============


meanAvgPrec :: forall rel qid . (Ord rel)
            => (qid -> TotalRel) -> rel -> ScoringMetric rel qid
meanAvgPrec totalRel relThresh rankings = 
    mean $ map perQuery $ M.toList $ rankings
  where
    perQuery (qid, ranking) =
        fromMaybe 0 $ avgPrec relThresh (totalRel qid) ranking



mean :: (RealFrac a) => [a] -> a
mean xs = sum xs / realToFrac (length xs)

naiveAvgPrec :: forall rel doc score. (Ord rel, VU.Unbox score)
             => rel       -- ^ threshold of relevance (inclusive)
             -> TotalRel  -- ^ total number of relevant documents
             -> Ranking score (doc, rel)  -- ^ ranking
             -> Maybe Double  -- ^ 'Nothing' if no relevant documents
naiveAvgPrec relThresh totalRel ranking
  | totalRel == 0 = Nothing
  | otherwise =
    let (_, relAtR) = mapAccumL numRelevantAt 0 rels

        rels :: [rel]
        rels = map (snd . snd) (Ranking.toSortedList ranking)

        numRelevantAt :: Int -> rel -> (Int, Int)
        numRelevantAt !accum rel
          | rel >= relThresh = let !accum' = accum + 1 in (accum', accum')
          | otherwise        = (accum, accum)

        precAtR :: [(Double, rel)]
        precAtR = zipWith3
                    (\n k rel -> let !prec = realToFrac n / realToFrac k
                                 in (prec, rel))
                    relAtR
                    [1 :: Int ..]
                    rels

        precAtRelevantRanks = [ prec
                              | (prec, rel) <- precAtR
                              , rel >= relThresh
                              ]
    in Just $! sum precAtRelevantRanks / realToFrac totalRel

avgPrec :: forall rel doc score. (Ord rel, VU.Unbox score)
        => rel       -- ^ threshold of relevance (inclusive)
        -> TotalRel  -- ^ total number of relevant documents
        -> Ranking score (doc, rel)  -- ^ ranking
        -> Maybe Double  -- ^ 'Nothing' if no relevant documents
avgPrec relThresh totalRel ranking
  | totalRel == 0 = Nothing
  | otherwise =
    let -- N.B. VG.indexed is zero-based but ranks should be one-based
        precsAtR = VG.imap (\i (r, _) -> realToFrac (i+1) / realToFrac (r+1)) relevantEnts
        relevantEnts = VG.filter (\(_r, (_, rel)) -> rel >= relThresh)
                       $ VG.indexed (Ranking.toSortedItems ranking)
    in Just $ VG.sum precsAtR / realToFrac totalRel
{-# SPECIALISE
    avgPrec :: forall rel doc. (Ord rel)
            => rel
            -> TotalRel
            -> Ranking Double (doc, rel)
            -> Maybe Double
    #-}
{-# SPECIALISE
    avgPrec :: IsRelevant
            -> TotalRel
            -> Ranking Double (doc, IsRelevant)
            -> Maybe Double
    #-}
