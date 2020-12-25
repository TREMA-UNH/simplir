{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -O2 #-}

module SimplIR.HashedRanking
    ( -- * HashedRankings
      HashedRanking
      -- ** Construction
    , empty
      -- *** From lists
    , fromList
    -- , fromListK
    , fromSortedList
      -- *** From vectors
    -- , fromVector
    -- , fromVectorK
    , fromSortedVector
      -- ** Destruction
    -- , toSortedList
    -- , toSortedVector
    -- , toSortedItems
    , toList
      -- ** Transformation
    , mapHashedRanking
    , mapHashedRankingK
    -- , takeTop
    -- , mapMaybe
    -- , filter
    , rescore
      -- ** Info for Eval measures
    , bucketCounts  
    ) where

import Control.DeepSeq
import qualified Data.Maybe as Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Hybrid as VH
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Algorithms.Intro as Sort
import qualified Data.Map.Strict as M
import Data.Ord
import Prelude hiding (filter)
import Data.Foldable (Foldable(foldl'))
import Debug.Trace as Debug

-- | A convenient alias for shortening @SPECIALISE@ pragmas.
-- type V = VH.Vector VU.Vector V.Vector

type Bucket score a = VH.Vector VU.Vector V.Vector (score, a)
type BucketBuilder score a = [(score, a)]

-- | A nearly-sorted list of items and their scores
--  All elements are hashed into an ordered list of buckets
-- This induces a partial order of items across buckets.
-- Howver, there are no guarantees on the order of elements within a bucket.
newtype HashedRanking score a = HashedRanking (M.Map score (Bucket score a) )

type HashedRankingBuilder score a = M.Map score (BucketBuilder score a)


-- | toList emits a roughly-sorted ranking
toList :: (VG.Vector VU.Vector score) => HashedRanking score a -> [(score, a)]
toList (HashedRanking hashedRanking) =
    [ e
    | (_, lst ) <-  M.toDescList hashedRanking
    , e <- VG.toList lst
    ]


deriving instance (Eq score, Eq a, VU.Unbox score) => Eq (HashedRanking score a)
deriving instance (Show score, Show a, VU.Unbox score) => Show (HashedRanking score a)
deriving instance (Ord score, Read score, Read a, VU.Unbox score) => Read (HashedRanking score a)

-- instance NFData a => NFData (HashedRanking score a) where
--     rnf (HashedRanking v) = rnf $ VH.projectSnd v

-- instance Functor (HashedRanking score) where
--     fmap f (HashedRanking v) = HashedRanking $ VH.unsafeZip (VH.projectFst v) (fmap f $ VH.projectSnd v)

-- instance Foldable (HashedRanking score) where
--     foldMap f (HashedRanking v) = foldMap f $ VH.projectSnd v

-- instance Traversable (HashedRanking score) where
--     traverse f (HashedRanking v) = HashedRanking . VH.unsafeZip (VH.projectFst v) <$> traverse f (VH.projectSnd v)

-- sort :: (Ord score, VU.Unbox score) => V (score, a) -> V (score, a)
-- sort = VH.modify $ Sort.sortBy (comparing $ Down . fst)
-- {-# SPECIALISE sort :: V (Double, a) -> V (Double, a) #-}
-- {-# SPECIALISE sort :: V (Float, a) -> V (Float, a) #-}

-- partialSort :: (Ord score, VU.Unbox score) => Int -> V (score, a) -> V (score, a)
-- partialSort k v =
--     VH.modify (\xs -> Sort.partialSortBy (comparing $ Down . fst) xs k') v
--   where k' = min (VH.length v) k
--         -- Avoid vector-algorithms #27.
-- {-# SPECIALISE partialSort :: Int -> V (Double, a) -> V (Double, a) #-}
-- {-# SPECIALISE partialSort :: Int -> V (Float, a) -> V (Float, a) #-}

-- | An empty 'HashedRanking'.
empty :: (VU.Unbox score, Ord score, RealFrac score)
      => HashedRanking score a
empty = emptyWithBuckets [-1/0, 0, 1/0]


-- | An empty 'HashedRanking', preseeded with known buckets
emptyWithBuckets :: (VG.Vector VU.Vector score, Ord score) => [score]  -> HashedRanking score a
emptyWithBuckets scores =
    HashedRanking $ 
      M.fromList
      $ [ (score, VG.empty)
        | score <- scores
        ]

-- | An empty 'HashedRanking', preseeded with known buckets
emptyBuilderWithBuckets :: (Ord score, Real score, Fractional score) => [score]  -> HashedRankingBuilder score a
emptyBuilderWithBuckets scores =
      M.fromList
      $ [ (score, [])
        | score <- (scores <> [1/0])
        ]



-- -- | @fromVector xs@ builds a ranking from entries of an unsorted vector @xs@.
-- fromVector :: (VU.Unbox score, Ord score)
--            => VH.Vector VU.Vector V.Vector (score, a)
--            -> HashedRanking score a
-- fromVector = HashedRanking . sort
-- {-# SPECIALISE fromVector :: V (Double, a) -> HashedRanking Double a #-}
-- {-# SPECIALISE fromVector :: V (Float, a) -> HashedRanking Float a #-}

-- -- | @fromVectorK k xs@ builds a ranking of the top \(k\) entries of an unsorted
-- -- vector @xs@.
-- fromVectorK :: (VU.Unbox score, Ord score)
--             => Int
--             -> VH.Vector VU.Vector V.Vector (score, a)
--             -> HashedRanking score a
-- fromVectorK 0 _ = empty
-- fromVectorK k v = HashedRanking $ VH.take k $ partialSort k v
-- {-# SPECIALISE fromVectorK :: Int -> V (Double, a) -> HashedRanking Double a #-}
-- {-# SPECIALISE fromVectorK :: Int -> V (Float, a) -> HashedRanking Float a #-}

-- | @fromSortedVector xs@ builds a ranking from entries of a sorted vector @xs@.
fromSortedVector :: forall score a. (VU.Unbox score, Ord score, Real score, Fractional score)
                 => VH.Vector VU.Vector V.Vector (score, a)
                 -> HashedRanking score a
fromSortedVector vec =
  fromVector vec


fromVector :: forall score a. (VU.Unbox score, Ord score, Real score, Fractional score)
                 => VH.Vector VU.Vector V.Vector (score, a)
                 -> HashedRanking score a
fromVector vec =
  let elements :: BucketBuilder score a
      elements = VG.toList vec
      total :: Int
      total = VH.length vec
      -- scoreVec :: VG.Vector score
      scoreVec = VH.projectFst vec
      offsets :: [Int] 
      offsets =  fmap (total `div` ) $ [1..3]
      bla = Debug.trace (show offsets) $ ()
      -- buckets :: [score]
      -- buckets = [ score
      --           | offset <- offsets
      --           , let (score, _) = vec `VG.!` offset
      --           ]
      (anyScore:_) = VU.toList $ VU.take 1 scoreVec
      buckets :: [score]
      buckets = (VU.toList $ VU.take 3 scoreVec) ++ ([anyScore/0])
      emptyHashedRanking :: HashedRankingBuilder score a         
      emptyHashedRanking = emptyBuilderWithBuckets buckets
      builder :: HashedRankingBuilder score a         
      builder = foldl' (\accum (score, e)  -> insert score e accum ) emptyHashedRanking $ elements         

      filledHashedRanking = HashedRanking $ M.map VG.fromList builder
  in filledHashedRanking

  where -- | insert a single scored element into the hashed ranking. 
        -- | (Warm up test code ... delete?)
        insert :: forall score a . (Ord score) 
              => score ->  a  -> HashedRankingBuilder score a -> HashedRankingBuilder score a
        insert score e hashedRanking =
              case M.lookupGT score hashedRanking of
                Nothing -> error $ " larger than any key."
                Just (key, _ ) -> M.alter (prependBucket score e)  key hashedRanking  

          where prependBucket :: score -> a -> Maybe (BucketBuilder score a) -> Maybe (BucketBuilder score a)
                prependBucket score1 elem1 (Just lst) = Just ( (score1,elem1) : lst)
                prependBucket score1 elem1 Nothing = Just ([(score1, elem1)])




-- | @fromList xs@ builds a ranking from entries of an unsorted list @xs@.
fromList :: (VU.Unbox score, Ord score, Real score,Fractional score) => [(score, a)] -> HashedRanking score a
fromList list =
  fromVector $ VH.fromList list
  

-- fromList = fromVector . VH.fromList
-- {-# SPECIALISE fromList :: [(Double, a)] -> HashedRanking Double a #-}
-- {-# SPECIALISE fromList :: [(Float, a)] -> HashedRanking Float a #-}

-- -- | @fromListK k xs@ builds a ranking of the top \(k\) entries of an unsorted
-- -- list @xs@.
-- fromListK :: (VU.Unbox score, Ord score) => Int -> [(score, a)] -> HashedRanking score a
-- fromListK k = fromVectorK k . VH.fromList
-- {-# SPECIALISE fromListK :: Int -> [(Double, a)] -> HashedRanking Double a #-}
-- {-# SPECIALISE fromListK :: Int -> [(Float, a)] -> HashedRanking Float a #-}

-- | @fromSortedList xs@ builds a ranking from entries of a sorted list @xs@.
fromSortedList :: (VU.Unbox score, Ord score, Real score,Fractional score) => [(score, a)] -> HashedRanking score a
fromSortedList list = fromList list

-- toSortedList :: (VU.Unbox score) => HashedRanking score a -> [(score, a)]
-- toSortedList (HashedRanking xs) = VH.toList xs
-- {-# SPECIALISE toSortedList :: HashedRanking Double a -> [(Double, a)] #-}
-- {-# SPECIALISE toSortedList :: HashedRanking Float a -> [(Float, a)] #-}

-- toSortedVector :: (VU.Unbox score) => HashedRanking score a -> VH.Vector VU.Vector V.Vector (score, a)
-- toSortedVector (HashedRanking xs) = xs

-- toSortedItems :: HashedRanking score a -> V.Vector a
-- toSortedItems (HashedRanking xs) = VH.projectSnd xs

-- -- | 'takeTop k xs' truncates 'HashedRanking' @xs@ at rank \(k\).
-- takeTop :: (VU.Unbox score) => Int -> HashedRanking score a -> HashedRanking score a
-- takeTop k (HashedRanking ranking) = HashedRanking (VH.take k ranking)

-- -- | Filter a ranking.
-- filter :: (VU.Unbox score) => (a -> Bool) -> HashedRanking score a -> HashedRanking score a
-- filter f (HashedRanking xs) = HashedRanking $ VH.filter (f . snd) xs

-- -- | Maps over the entries of a 'HashedRanking', allowing some to be dropped.
-- mapMaybe :: (VU.Unbox score) => (a -> Maybe b) -> HashedRanking score a -> HashedRanking score b
-- mapMaybe f = fromSortedList . Maybe.mapMaybe g . toSortedList
--   where g (s,x) = fmap (\y -> (s,y)) (f x)

-- | Map both scores and items, resorting afterwards to ensure order.
mapHashedRanking :: (VU.Unbox score, VU.Unbox score', Ord score', Real score',Fractional score')
           => (score -> a -> (score', b))
           -> HashedRanking score  a
           -> HashedRanking score' b
mapHashedRanking f hashedRanking =  fromList $ fmap (uncurry f) $ toList hashedRanking  
{-# SPECIALISE mapHashedRanking :: (Double -> a -> (Double, b))
                          -> HashedRanking Double a -> HashedRanking Double b #-}
{-# SPECIALISE mapHashedRanking :: (Float -> a -> (Float, b))
                          -> HashedRanking Float a -> HashedRanking Float b #-}

mapHashedRankingK :: (VU.Unbox score, VU.Unbox score', Ord score', Real score',Fractional score')
            => Int
            -> (score -> a -> (score', b))
            -> HashedRanking score  a
            -> HashedRanking score' b
mapHashedRankingK _k f hashedRanking =
    mapHashedRanking f hashedRanking 



{-# SPECIALISE mapHashedRankingK :: Int -> (Double -> a -> (Double, b))
                           -> HashedRanking Double a -> HashedRanking Double b #-}
{-# SPECIALISE mapHashedRankingK :: Int -> (Float -> a -> (Float, b))
                           -> HashedRanking Float a -> HashedRanking Float b #-}

-- | Recompute a 'HashedRanking'\'s scores.
rescore :: (VU.Unbox score, VU.Unbox score', Ord score', Real score',Fractional score')
        => (a -> (score', b))
        -> HashedRanking score  a
        -> HashedRanking score' b
rescore f = mapHashedRanking (const f)
{-# INLINE rescore #-}


bucketCounts :: (VG.Vector VU.Vector score) 
             => (a -> Bool) -> HashedRanking score a -> [(score, (Int,Int))]
bucketCounts  isRel (HashedRanking hashedRanking) =
    [ (key, (n,r))
    | (key, bucket) <- M.toDescList hashedRanking
    , let n = VG.length bucket
    , let r = VG.length $ VG.filter (\(_, a) -> isRel a) bucket
    ]
