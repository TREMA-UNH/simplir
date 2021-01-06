{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module SimplIR.LearningToRank
    ( -- * Basic types
      Score
    , Ranking
    , HashedRanking
    , TotalRel
    , WeightVec(..)
    , score
      -- * Features
      -- * Computing rankings
    , rescoreList
    , rerank  
      -- * Scoring metrics
    , ScoringMetric, ApproxScoringMetric, AbstractScoringMetric
    , produceRankingsForEval, produceHashedRankingsForEval
    , meanAvgPrec, meanAvgPrecApprox
      -- * Learning
    , FRanking
      -- ** Coordinate ascent optimisation
    , coordAscent
    , coordAscentHashed
    , EvalCutoff(..)
    -- , naiveCoordAscent
    , naiveCoordAscent'
      -- ** Mini-batching
    , miniBatched
    , miniBatchedAndEvaluated
    , defaultMiniBatchParams, MiniBatchParams(..)
      -- * Helpers
    , IsRelevant(..)
    ) where

import GHC.Generics
import Control.DeepSeq
import Control.Parallel.Strategies
import Data.Ord
import Data.List
import qualified System.Random as Random
import System.Random.Shuffle
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as VU
import Linear.Epsilon

import qualified SimplIR.FeatureSpace as FS
import SimplIR.FeatureSpace (FeatureVec)
import SimplIR.Types.Relevance
import SimplIR.Ranking as Ranking
import SimplIR.HashedRanking as HashedRanking
import SimplIR.Ranking.Evaluation
import SimplIR.TrainUtils
import Data.Aeson (FromJSON)
import Data.Aeson.Types (ToJSON)

type Score = Double

newtype WeightVec f s = WeightVec { getWeightVec :: FeatureVec f s Double }
                    deriving stock (Show, Generic)
                    deriving newtype (NFData)

-- | Returns 'Nothing' if vector is near zero magnitude.
l2NormalizeWeightVec :: WeightVec f s -> Maybe (WeightVec f s)
l2NormalizeWeightVec (WeightVec w)
  | nearZero norm = Nothing
  | otherwise     = Just $ WeightVec $ recip norm `FS.scale` w
  where norm = FS.l2Norm w

stepFeature :: Step s -> WeightVec f s -> WeightVec f s
stepFeature (Step dim delta) (WeightVec v) =
    let x = v `FS.lookupIndex` dim
        !x' = x + delta
    in WeightVec $ v `FS.modifyIndices` [(dim, x')]

-- | All necessary data to produce a ranking of documents (along with relevance annotations), given the weight parameter
type FRanking f s relevance a = [(a, FeatureVec f s Double, relevance)]




score :: WeightVec f s -> FeatureVec f s Double -> Score
score (WeightVec w) f = w `FS.dot` f

-- | Re-rank a set of documents given a weight vector.
rerank :: WeightVec f s -> [(a, FeatureVec f s Double)] -> Ranking Score a
rerank weight fRanking =
    Ranking.fromList
    [ (weight `score` feats, doc)
    | (doc, feats) <- fRanking
    ]

-- | create new HashedRanking from a new unsorted ranking
reHashRank :: WeightVec f s -> [(a, FeatureVec f s Double)] -> HashedRanking Score a
reHashRank weight fRanking = 
    HashedRanking.fromList
    [ (weight `score` feats, doc)
    | (doc, feats) <- fRanking
    ]  

rescoreList :: WeightVec f s -> [(a, FeatureVec f s Double)] -> [(Score, a)]
rescoreList  weight fRanking = 
    [ (weight `score` feats, doc)
    | (doc, feats) <- fRanking
    ]  
-- # INline?!?!


data Step s = Step !(FS.FeatureIndex s) Double
     deriving Show

isZeroStep :: Step s -> Bool
isZeroStep (Step _ d) = d == 0

-- | @dotStepOracle w f step == (w + step) `dot` f@.
-- produces scorers that make use of the original dot product - only applying local modifications induced by the step
scoreStepOracle :: forall f s. WeightVec f s -> FeatureVec f s Double -> (Step s -> Score)
scoreStepOracle w f = scoreFun
  where
    scoreFun :: Step s -> Score
    scoreFun s | isZeroStep s = score0
    scoreFun (Step dim delta) = score0 - scoreTerm dim 0 + scoreTerm dim delta

    -- scoreDeltaTerm: computes term contribution to dot product of w' + step
    scoreTerm dim off = (off + getWeightVec w `FS.lookupIndex` dim) * (f `FS.lookupIndex` dim)
    !score0 = w `score` f

data MiniBatchParams = MiniBatchParams { miniBatchParamsBatchSteps :: Int   -- ^ iterations per mini-batch
                                       , miniBatchParamsBatchSize :: Int    -- ^ mini-batch size
                                       , miniBatchParamsEvalSteps :: Int    -- ^ mini-batches per evaluation cycle
                                       }
  deriving (Show, Generic, FromJSON, ToJSON)
  
defaultMiniBatchParams :: MiniBatchParams
defaultMiniBatchParams = MiniBatchParams 1 100 0

miniBatchedAndEvaluated
    :: forall a f s qid relevance gen scoringMetric r.
       (Random.RandomGen gen, Show qid, Ord qid, Show a, Show relevance, Show f)
    => MiniBatchParams
    -> AbstractScoringMetric r relevance qid
       -- ^ evaluation metric
    -> ProduceRankingsForEval r a relevance
       -- ^ produceRankings
    -> (gen -> WeightVec f s -> M.Map qid (FRanking f s relevance a) -> [WeightVec f s])
       -- ^ optimiser (e.g. 'coordAscent')
    -> gen
    -> WeightVec f s  -- ^ initial weights
    -> M.Map qid (FRanking f s relevance a)
    -> [(Score, WeightVec f s)]
       -- ^ list of evaluation iterates with evaluation metric
miniBatchedAndEvaluated (MiniBatchParams batchSteps batchSize evalSteps) evalMetric produceRankings
                        optimise gen00 w00 fRankings =
    go $ miniBatched batchSteps batchSize optimise gen00 w00 fRankings
  where
    -- shuffle tuples for 'rerank'
    fRankings' = fmap (map (\(doc,feats,rel) -> ((doc,rel),feats))) fRankings
    go iters =
        let w:rest = drop evalSteps iters
            rankings ::  M.Map qid (r Double (a, relevance))
            rankings = M.map (produceRankings .(rescoreList w)) fRankings'
        in (evalMetric (rankings), w) : go rest  -- ## ToDo: want to get rid of "M.map Ranking.fromList" (move into eval metric!)


-- ##### old

-- miniBatchedAndEvaluated
--     :: forall a f s qid relevance gen.
--        (Random.RandomGen gen, Show qid, Ord qid, Show a, Show relevance, Show f)
--     => MiniBatchParams
--     -> GenericScoringMetric relevance qid
--        -- ^ evaluation metric
--     -> (gen -> WeightVec f s -> M.Map qid (FRanking f s relevance a) -> [WeightVec f s])
--        -- ^ optimiser (e.g. 'coordAscent')
--     -> gen
--     -> WeightVec f s  -- ^ initial weights
--     -> M.Map qid (FRanking f s relevance a)
--     -> [(Score, WeightVec f s)]
--        -- ^ list of evaluation iterates with evaluation metric
-- miniBatchedAndEvaluated (MiniBatchParams batchSteps batchSize evalSteps) evalMetric 
--                         optimise gen00 w00 fRankings =
--     go $ miniBatched batchSteps batchSize optimise gen00 w00 fRankings
--   where
--     -- shuffle tuples for 'rerank'
--     fRankings' = fmap (map (\(doc,feats,rel) -> ((doc,rel),feats))) fRankings

--     go :: [WeightVec f s] -> [(Score, WeightVec f s)]
--     go iters =
--         let w:rest = drop evalSteps iters

--             rankings :: M.Map qid ([(Score, (a, relevance))])
--             rankings = fmap (rescoreList w) fRankings'
--         in (evalMetric (rankings), w) : go rest  -- ## ToDo: want to get rid of "M.map Ranking.fromList" (move into eval metric!)
--         -- in (evalMetric (M.map Ranking.fromList rankings), w) : go rest  -- ## ToDo: want to get rid of "M.map Ranking.fromList" (move into eval metric!)

-- #################


-- naiveCoordAscent
--     :: forall a f s qid d gen relevance.
--        (Random.RandomGen gen, Show qid, Show a, Show f)
--     => ScoringMetric relevance qid
--     -> (d -> WeightVec f s -> Ranking Double (a,relevance))
--        -- ^ re-ranking function
--     -> gen
--     -> WeightVec f s           -- ^ initial weights
--     -> M.Map qid d             -- ^ training data
--     -> [(Score, WeightVec f s)]  -- ^ list of iterates
-- naiveCoordAscent scoreRanking rerank gen0 w0 fRankings =
--     naiveCoordAscent' l2NormalizeWeightVec obj gen0 w0
--   where
--     obj w = scoreRanking $ withStrategy (parTraversable $ evalTraversable rseq) $ fmap (\d -> rerank d w) fRankings


deltas :: RealFrac a => [a]
deltas = [ f x
         | x <- [0.0001 * 2^n | n <- [1..20::Int]]
         , f <- [id, negate]
         ] ++ [0]

-- | Maximization via coordinate ascent.
naiveCoordAscent'
    :: forall f s gen.
       (Random.RandomGen gen, Show f)
    => (WeightVec f s -> Maybe (WeightVec f s)) -- ^ normalization
    -> (WeightVec f s -> Double)                -- ^ objective
    -> gen
    -> WeightVec f s                            -- ^ initial weights
    -> [(Double, WeightVec f s)]                -- ^ list of iterates
naiveCoordAscent' normalise obj gen0 w0
 | null (FS.featureIndexes fspace)  = error "naiveCoordAscent': Empty feature space"
 | otherwise =
    let Just w0' = normalise w0
    in go gen0 (obj w0', w0')
  where
    fspace = FS.featureSpace $ getWeightVec w0
    dim = FS.dimension fspace

    go :: gen -> (Score, WeightVec f s) -> [(Score, WeightVec f s)]
    go gen (s,w) = (s',w') : go gen' (s',w')
      where
        !(s', w') = foldl' updateDim (s,w) dims
        dims = shuffle' (FS.featureIndexes fspace) dim g
        (g, gen') = Random.split gen

    updateDim :: (Score, WeightVec f s) -> FS.FeatureIndex s -> (Score, WeightVec f s)
    updateDim (score0, w0) dim
      | null steps = --Debug.trace ("coord dim "<> show dim<> " show score0 "<> show score0 <> ": No valid steps in ") $
                     (score0, w0)
      | otherwise  = --Debug.trace ("coord dim "<> show dim<> " show score0 "<> show score0 <> ": Found max: ") $
                    maximumBy (comparing fst) steps
      where
        steps :: [(Score, WeightVec f s)]
        steps =
            [ (score, w')
            | delta <- deltas
            , let step = Step dim delta
            , Just w' <- pure $ normalise $ stepFeature step w0
            , let score = obj w'
            , not $ isNaN score
            ]

-- | 'coordAscent' can optionally truncate rankings during evaluation. This
-- improves runtime complexity at the cost of a slight approximation.
data EvalCutoff = EvalNoCutoff
                | EvalCutoffAt !Int
  deriving (Show, Eq, Generic, FromJSON, ToJSON)


coordAscent :: forall a f s qid relevance gen.
               (Random.RandomGen gen, Show qid, Show a, Show f)
            => EvalCutoff               -- ^ ranking truncation point for scoring
            -> ScoringMetric relevance qid
            -> gen
            -> WeightVec f s            -- ^ initial weights
            -> M.Map qid (FRanking f s relevance a)
            -> [(Score, WeightVec f s)] -- ^ list of iterates
coordAscent evalCutoff scoreRanking gen0 w0 =
  coordAscentGeneric Ranking.rescore rerank Ranking.mapRanking Ranking.mapRankingK 
                  evalCutoff scoreRanking gen0 w0



coordAscentHashed :: forall a f s qid relevance gen.
               (Random.RandomGen gen, Show qid, Show a, Show f)
            => EvalCutoff               -- ^ ranking truncation point for scoring
            -> (forall a0. M.Map qid (HashedRanking Double (a0,relevance)) -> Double)
            -> gen
            -> WeightVec f s            -- ^ initial weights
            -> M.Map qid (FRanking f s relevance a)
            -> [(Score, WeightVec f s)] -- ^ list of iterates
coordAscentHashed evalCutoff scoreRanking gen0 w0 =
  coordAscentGeneric HashedRanking.rescore reHashRank HashedRanking.mapHashedRanking HashedRanking.mapHashedRankingK
                     evalCutoff scoreRanking gen0 w0



coordAscentGeneric :: forall r a f s qid relevance gen.
               (Random.RandomGen gen, Show qid, Show a, Show f)
            => (((Step s -> Score,
                    ((a, FeatureVec f s Double), relevance))
                   -> (Score, ((a, FeatureVec f s Double), relevance)))
                  -> r Score
                       (Step s -> Score, ((a, FeatureVec f s Double), relevance))
                  -> r Score ((a, FeatureVec f s Double), relevance))
                -- ^ rescore_r

            -> (WeightVec f s
                  -> [(((a, FeatureVec f s Double), relevance),
                       FeatureVec f s Double)]
                  -> r Score ((a, FeatureVec f s Double), relevance)) 
               -- ^ rerank_r

             -> (forall score0 b score1 b'. (VU.Unbox score0, VU.Unbox score1, Real score0, Real score1, Ord score1, Fractional score0, Fractional score1)   => (score0 -> b -> (score1, b')) -> r score0 b -> r score1 b')
               -- ^ mapRanking_r

             -> (forall score0 b score1 b'. (VU.Unbox score0, VU.Unbox score1, Real score0, Real score1, Ord score1, Fractional score0, Fractional score1)   => Int -> (score0 -> b -> (score1, b')) -> r score0 b -> r score1 b')
              -- ^ mapRankingK_r

            --  -> [(Double, a)] -> r Double a 
            --   -- ^ listToRanking

            -> EvalCutoff               -- ^ ranking truncation point for scoring
            -> (forall a0. M.Map qid (r Double (a0,relevance)) -> Double)
            -> gen
            -> WeightVec f s            -- ^ initial weights
            -> M.Map qid (FRanking f s relevance a)
            -> [(Score, WeightVec f s)] -- ^ list of iterates
coordAscentGeneric 
       rescore_r rerank_r  mapRanking_r mapRankingK_r -- listToRanking
       evalCutoff scoreRanking gen0 w0
  | null (FS.featureIndexes fspace)  = error "coordAscent: Empty feature space"
  | otherwise = \fRankings ->
      let Just w0' = l2NormalizeWeightVec w0
          score0 = scoreRanking fRankings'
          fRankings' = fmap (rerank_r w0' . map (\(doc, feats, rel) -> (((doc, feats), rel), feats))) fRankings
      in if isNaN score0
         then error "coordAscent: Initial score is not a number"
         else go gen0 (score0, w0', fRankings')
  where
    fspace = FS.featureSpace $ getWeightVec w0
    dim = FS.dimension fspace

    zeroStep :: Step s
    zeroStep = Step (head $ FS.featureIndexes fspace) 0

    go :: gen
       -> (Score, WeightVec f s, M.Map qid (r Score ((a, FeatureVec f s Double), relevance)))
       -> [(Score, WeightVec f s)]
    go gen state = (score, w') : go gen' state'
      where
        state'@(score, !w', _fRankings') = foldl' updateDim state dims
        dims = shuffle' (FS.featureIndexes fspace) dim g
        (g, gen') = Random.split gen

    updateDim :: (Score, WeightVec f s, M.Map qid (r Score ((a, FeatureVec f s Double), relevance)))
              -> FS.FeatureIndex s
              -> (Score, WeightVec f s, M.Map qid (r Score ((a, FeatureVec f s Double), relevance)))
    updateDim (score0, w0, fRankings) dim
      | null steps = (score0, w0, fRankings)
      | otherwise  =
          let (score, step) = maximumBy (comparing fst) steps
              Just w = l2NormalizeWeightVec $ stepFeature step w0
          in (score, w, updateRankings (stepScorerRankings EvalNoCutoff) step)
      where
        steps :: [(Score, Step s)]
        steps =
            [ if isNaN score
              then error "coordAscent: Score is NaN"
              else (score, step)
              -- possibly l2Normalize requires to invalidate scoredRaking caches
              -- and a full recomputation of the scores
            | delta <- deltas
            , let step = Step dim delta
            , Just _ <- pure $ l2NormalizeWeightVec $ stepFeature step w0
            , let !rankings = updateRankings cachedScoredRankings step
            , let score = scoreRanking rankings
            ]

        updateRankings :: M.Map qid (r Score (Step s -> Score, ((a, FeatureVec f s Double), relevance)))
                       -> Step s
                       -> M.Map qid (r Score ((a, FeatureVec f s Double), relevance))
        updateRankings rankings step =
            fmap (\ranking -> rescore_r scoreDoc ranking) rankings
          where
            scoreDoc :: (Step s -> Score, ((a, FeatureVec f s Double), relevance))
                     -> (Score, ((a, FeatureVec f s Double), relevance))
            scoreDoc (stepScorer, item) = (stepScorer step, item)

        !cachedScoredRankings = stepScorerRankings evalCutoff

        mapEvalRanking :: forall score0 b score1 b'. (VU.Unbox score0, VU.Unbox score1, Ord score1, Real score0, Real score1, Fractional score0, Fractional score1)
                       => EvalCutoff
                       -> (score0 -> b -> (score1, b'))
                       -> r score0 b -> r score1 b'
        mapEvalRanking (EvalCutoffAt k) = mapRankingK_r k
        mapEvalRanking EvalNoCutoff = mapRanking_r

        stepScorerRankings :: EvalCutoff
                           -> M.Map qid (r Score (Step s -> Score, ((a, FeatureVec f s Double), relevance)))
        stepScorerRankings cutoff =
            fmap augment fRankings
          where
            -- Attach a step-scorer to each document in a ranking.
            augment :: r Score ((a, FeatureVec f s Double), relevance)
                    -> r Score (Step s -> Score, ((a, FeatureVec f s Double), relevance))
            augment = mapEvalRanking cutoff updateScorer
              where
                -- Recompute the scoreFun and the document's current score.
                updateScorer :: Score
                             -> ((a, FeatureVec f s Double), relevance)
                             -> (Score, (Step s -> Score, ((a, FeatureVec f s Double), relevance)))
                updateScorer _ things@((_, fVec), _) = (stepScorer zeroStep, (stepScorer, things))
                  where !stepScorer = scoreStepOracle w0 fVec

