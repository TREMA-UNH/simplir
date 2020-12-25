{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -O2 #-}

module SimplIR.Ranking
    ( -- * Rankings
      Ranking
      -- ** Construction
    , empty
      -- *** From lists
    , fromList
    , fromListK
    , fromSortedList
      -- *** From vectors
    , fromVector
    , fromVectorK
    , fromSortedVector
      -- ** Destruction
    , toSortedList
    , toSortedVector
    , toSortedItems
      -- ** Transformation
    , mapRanking
    , mapRankingK
    , takeTop
    , mapMaybe
    , filter
    , rescore
    ) where

import Control.DeepSeq
import qualified Data.Maybe as Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Hybrid as VH
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Algorithms.Intro as Sort
import Data.Ord
import Prelude hiding (filter)

-- | A convenient alias for shortening @SPECIALISE@ pragmas.
type V = VH.Vector VU.Vector V.Vector

-- | A sorted list of items and their scores
newtype Ranking score a = Ranking (VH.Vector VU.Vector V.Vector (score, a))

deriving instance (Eq score, Eq a, VU.Unbox score) => Eq (Ranking score a)
deriving instance (Show score, Show a, VU.Unbox score) => Show (Ranking score a)
deriving instance (Read score, Read a, VU.Unbox score) => Read (Ranking score a)

instance NFData a => NFData (Ranking score a) where
    rnf (Ranking v) = rnf $ VH.projectSnd v

instance Functor (Ranking score) where
    fmap f (Ranking v) = Ranking $ VH.unsafeZip (VH.projectFst v) (fmap f $ VH.projectSnd v)

instance Foldable (Ranking score) where
    foldMap f (Ranking v) = foldMap f $ VH.projectSnd v

instance Traversable (Ranking score) where
    traverse f (Ranking v) = Ranking . VH.unsafeZip (VH.projectFst v) <$> traverse f (VH.projectSnd v)

sort :: (Ord score, VU.Unbox score) => V (score, a) -> V (score, a)
sort = VH.modify $ Sort.sortBy (comparing $ Down . fst)
{-# SPECIALISE sort :: V (Double, a) -> V (Double, a) #-}
{-# SPECIALISE sort :: V (Float, a) -> V (Float, a) #-}

partialSort :: (Ord score, VU.Unbox score) => Int -> V (score, a) -> V (score, a)
partialSort k v =
    VH.modify (\xs -> Sort.partialSortBy (comparing $ Down . fst) xs k') v
  where k' = min (VH.length v) k
        -- Avoid vector-algorithms #27.
{-# SPECIALISE partialSort :: Int -> V (Double, a) -> V (Double, a) #-}
{-# SPECIALISE partialSort :: Int -> V (Float, a) -> V (Float, a) #-}

-- | An empty 'Ranking'.
empty :: (VU.Unbox score)
      => Ranking score a
empty = Ranking VH.empty

-- | @fromVector xs@ builds a ranking from entries of an unsorted vector @xs@.
fromVector :: (VU.Unbox score, Ord score)
           => VH.Vector VU.Vector V.Vector (score, a)
           -> Ranking score a
fromVector = Ranking . sort
{-# SPECIALISE fromVector :: V (Double, a) -> Ranking Double a #-}
{-# SPECIALISE fromVector :: V (Float, a) -> Ranking Float a #-}

-- | @fromVectorK k xs@ builds a ranking of the top \(k\) entries of an unsorted
-- vector @xs@.
fromVectorK :: (VU.Unbox score, Ord score)
            => Int
            -> VH.Vector VU.Vector V.Vector (score, a)
            -> Ranking score a
fromVectorK 0 _ = empty
fromVectorK k v = Ranking $ VH.take k $ partialSort k v
{-# SPECIALISE fromVectorK :: Int -> V (Double, a) -> Ranking Double a #-}
{-# SPECIALISE fromVectorK :: Int -> V (Float, a) -> Ranking Float a #-}

-- | @fromSortedVector xs@ builds a ranking from entries of a sorted vector @xs@.
fromSortedVector :: (VU.Unbox score)
                 => VH.Vector VU.Vector V.Vector (score, a)
                 -> Ranking score a
fromSortedVector = Ranking


-- | @fromList xs@ builds a ranking from entries of an unsorted list @xs@.
fromList :: (VU.Unbox score, Ord score) => [(score, a)] -> Ranking score a
fromList = fromVector . VH.fromList
{-# SPECIALISE fromList :: [(Double, a)] -> Ranking Double a #-}
{-# SPECIALISE fromList :: [(Float, a)] -> Ranking Float a #-}

-- | @fromListK k xs@ builds a ranking of the top \(k\) entries of an unsorted
-- list @xs@.
fromListK :: (VU.Unbox score, Ord score) => Int -> [(score, a)] -> Ranking score a
fromListK k = fromVectorK k . VH.fromList
{-# SPECIALISE fromListK :: Int -> [(Double, a)] -> Ranking Double a #-}
{-# SPECIALISE fromListK :: Int -> [(Float, a)] -> Ranking Float a #-}

-- | @fromSortedList xs@ builds a ranking from entries of a sorted list @xs@.
fromSortedList :: (VU.Unbox score) => [(score, a)] -> Ranking score a
fromSortedList = fromSortedVector . VH.fromList


toSortedList :: (VU.Unbox score) => Ranking score a -> [(score, a)]
toSortedList (Ranking xs) = VH.toList xs
{-# SPECIALISE toSortedList :: Ranking Double a -> [(Double, a)] #-}
{-# SPECIALISE toSortedList :: Ranking Float a -> [(Float, a)] #-}

toSortedVector :: (VU.Unbox score) => Ranking score a -> VH.Vector VU.Vector V.Vector (score, a)
toSortedVector (Ranking xs) = xs

toSortedItems :: Ranking score a -> V.Vector a
toSortedItems (Ranking xs) = VH.projectSnd xs

-- | 'takeTop k xs' truncates 'Ranking' @xs@ at rank \(k\).
takeTop :: (VU.Unbox score) => Int -> Ranking score a -> Ranking score a
takeTop k (Ranking ranking) = Ranking (VH.take k ranking)

-- | Filter a ranking.
filter :: (VU.Unbox score) => (a -> Bool) -> Ranking score a -> Ranking score a
filter f (Ranking xs) = Ranking $ VH.filter (f . snd) xs

-- | Maps over the entries of a 'Ranking', allowing some to be dropped.
mapMaybe :: (VU.Unbox score) => (a -> Maybe b) -> Ranking score a -> Ranking score b
mapMaybe f = fromSortedList . Maybe.mapMaybe g . toSortedList
  where g (s,x) = fmap (\y -> (s,y)) (f x)

-- | Map both scores and items, resorting afterwards to ensure order.
mapRanking :: (VU.Unbox score, VU.Unbox score', Ord score')
           => (score -> a -> (score', b))
           -> Ranking score  a
           -> Ranking score' b
mapRanking f (Ranking xs) = Ranking $ sort $ VH.map (uncurry f) xs
{-# SPECIALISE mapRanking :: (Double -> a -> (Double, b))
                          -> Ranking Double a -> Ranking Double b #-}
{-# SPECIALISE mapRanking :: (Float -> a -> (Float, b))
                          -> Ranking Float a -> Ranking Float b #-}

mapRankingK :: (VU.Unbox score, VU.Unbox score', Ord score')
            => Int
            -> (score -> a -> (score', b))
            -> Ranking score  a
            -> Ranking score' b
mapRankingK k f (Ranking xs) = Ranking $ VH.take k $ partialSort k $ VH.map (uncurry f) xs
{-# SPECIALISE mapRankingK :: Int -> (Double -> a -> (Double, b))
                           -> Ranking Double a -> Ranking Double b #-}
{-# SPECIALISE mapRankingK :: Int -> (Float -> a -> (Float, b))
                           -> Ranking Float a -> Ranking Float b #-}

-- | Recompute a 'Ranking'\'s scores.
rescore :: (VU.Unbox score, VU.Unbox score', Ord score')
        => (a -> (score', b))
        -> Ranking score  a
        -> Ranking score' b
rescore f = mapRanking (const f)
{-# INLINE rescore #-}

