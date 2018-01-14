{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module SimplIR.FeatureSpace where

import Data.List
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Map.Strict as M

newtype FeatureVec f a = FeatureVec (VU.Vector a)

newtype FeatureIndex f = FeatureIndex Int

data FeatureSpace f where
    -- | Space to create low level feature vectors
    Space :: V.Vector f
          -> M.Map f (FeatureIndex f)
          -> FeatureSpace f
    -- | Space as concatenation of feature vectors
    ConcatSpace :: (Ord f, Ord f') => FeatureSpace f -> FeatureSpace f' -> FeatureSpace (Either f f')

featureDimension :: FeatureSpace f -> Int
featureDimension (Space v _) = V.length v
featureDimension (ConcatSpace x x') = featureDimension x + featureDimension x'

featureNames :: FeatureSpace f -> [f]
featureNames (Space v _) = V.toList v
featureNames (ConcatSpace x x') = map Left (featureNames x) ++ map Right (featureNames x')

mkFeatureSpace :: Ord f => [f] -> FeatureSpace f
mkFeatureSpace xs = Space v m
  where
    m = M.fromList $ zip (sort xs) (map FeatureIndex [0..])
    v = V.fromList $ map fst $ M.toAscList m

concatSpace :: (Ord f, Ord f') => FeatureSpace f -> FeatureSpace f' -> FeatureSpace (Either f f')
concatSpace fs1 fs2 = ConcatSpace fs1 fs2

lookupName2Index :: Ord f => FeatureSpace f -> f -> FeatureIndex f
lookupName2Index (Space _ m)        x         = fromJust (error "feature not found") $ M.lookup x m
lookupName2Index (ConcatSpace f _) (Left x)   =
    case lookupName2Index f x of FeatureIndex i -> FeatureIndex i
lookupName2Index (ConcatSpace f' f) (Right x) =
    case lookupName2Index f x of FeatureIndex i -> FeatureIndex (featureDimension f' + i)


lookupIndex2Name :: FeatureSpace f -> FeatureIndex f -> f
lookupIndex2Name (Space v _) (FeatureIndex i) = v V.! i
lookupIndex2Name (ConcatSpace f f') (FeatureIndex i)
    | i < featureDimension f = Left $ lookupIndex2Name f (FeatureIndex i)
    | otherwise              = Right $ lookupIndex2Name f' (FeatureIndex (i - featureDimension f))


concatFeatureVec :: VU.Unbox a => FeatureVec f a -> FeatureVec f' a -> FeatureVec (Either f f') a
concatFeatureVec (FeatureVec v) (FeatureVec v') = FeatureVec (v VU.++ v')

fromList :: (VU.Unbox a, Ord f)
         =>  FeatureSpace f
         -> FeatureVec f a -- ^ default value
         -> [(f, a)]
         -> FeatureVec f a
fromList space (FeatureVec def) xs =
    FeatureVec $ VU.accum (const id) def
    [ (i,x)
    | (f,x) <- xs
    , let FeatureIndex i = lookupName2Index space f
    ]

toList :: VU.Unbox a => FeatureSpace f -> FeatureVec f a -> [(f, a)]
toList feats (FeatureVec vals) =
    zip (featureNames feats) (VU.toList vals)


data EntityFeatures = EntBM25 | EntDegree | EntQL
data EdgeFeatures = EdgeBM25 | EdgeCount
data EntityEdgeFeatures = TextSimBetweenEntityAndEdgeDoc
type CombinedFeatures = (EntityFeatures, EdgeFeatures, EntityEdgeFeatures)

--
-- main = do
--     let entFSpace = mkFeatureSpace [EntBM25, EntDegree, EntQL]
--         edgeFSpace = mkFeatureSpace [EdgeBM25, EdgeCount]
--         combinedFspace = entFSpace `concatSpace` edgeFSpace
--
--     let graph :: Graph n e
--         graph = ...
--
--         edgeFeatures :: HM.HashMap e (FeatureVec EdgeFeatures Double)
--         edgeFeatures = computeEdgeFeatures edgeFeatures
--
--         combined :: HM.HashMap n (FeatureVec CombinedFeatures Double)
--         combined =
--             [ concatFeatureVec entF edgeF
--             | n <- nodes graph
--             , let entF = computeEntityFeatures entFSpace n
--                   edgeF = marginalize (incidentEdges graph n)
--                   entityEdgeF = marginalize [ computeEntityEdgeFeatures n e
--                                             | e <- incidentEdges
--                                             ]
--             ]
--
--     let -- learn model
--         model :: FeatureVec CombinedFeatures Double
--         model = ...
--
--         weights = toList combinedFspace model
--
--
--
-- computeEntityFeatures fspace =
--     let entityValues = ... :: [(EntityFeatures, Double)]
--         vec = fromFeatureVec fspace xs :: FeatureVec EntityFeature s Double
--
-- computeEdgeFeatures fspace =
--         edgeValues = ... :: [(EdgeFeatures, Double)]
--         vec = fromFeatureVec fspace xs :: FeatureVec EdgeFeatures s Double
--     in ...
--
--
--
--
-- class FeatureDim f where
--     defaultFeature :: f -> Double
--

--------------------------------------------------------------------------------
