{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -O2 #-}

module SimplIR.FeatureSpace
    ( FeatureIndex
    -- * Feature spaces
    , SomeFeatureSpace(..)
    , FeatureSpace
    -- ** Construction
    , mkFeatureSpace
    , eitherSpaces
    , concatSpaces
    -- *** Unsafe
    , unsafeFromFeatureList
    -- ** Queries
    , dimension
    , featureNames
    , featureNameSet
    , featureIndexBounds
    , featureIndexes
    , lookupFeatureIndex
    , lookupFeatureName
    , internFeatureName
    -- ** Manipulating feature spaces
    , mapFeatureNames
    -- * Feature vectors
    , FeatureVec
    , featureSpace
    -- ** Construction
    , generate
    , repeat
    , fromList
    , aggregateWith
    , mkFeaturesF
    -- *** Unsafe
    , unsafeFromVector
    -- *** Stacking
    , Stack
    , FeatureStack(..)
    , stack
    -- ** Destruction
    , toList
    , toVector
    -- ** Lookups
    , lookup
    , lookupIndex
    -- ** Mapping and zipping
    , map
    , zipWith
    -- *** Generalized mapping
    , FeatureMapping(..)
    , mapFeatures
    , FeatureMappingInto(..)
    , mapFeaturesInto
    -- ** Updates
    , modifyIndices
    , modify
    , accum
    -- ** Algebra
    , scale
    , (^+^), (^-^), (^*^), (^/^), sum, l2Norm
    , dot
    -- ** Mapping between spaces
    , project
    , projectBoth
    , ProjectBothResult(..)
    , equivSpace
    , eqSpace
    ) where

import Control.DeepSeq
import Control.Monad hiding (fail)
import Control.Monad.Fail
import Control.Monad.Primitive
import Control.Monad.Trans.Except
import Control.Monad.ST
import Data.Bifunctor
import Data.Coerce
import Data.Foldable (forM_)
import qualified Control.Foldl as F
import Data.Kind
import Data.Type.Equality
import Data.Ix
import qualified Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NE
import Data.Tuple
import Data.Maybe
import GHC.Stack
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector.Indexed as VI
import qualified Data.Vector.Indexed.Mutable as VIM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector as V

import Prelude hiding (map, zipWith, repeat, sum, lookup, fail)

newtype FeatureIndex s = FeatureIndex { getFeatureIndex :: Int }
                       deriving (Show, Ord, Eq, Enum, Ix)

type role FeatureIndex nominal

data SomeFeatureSpace f where
    SomeFeatureSpace :: (forall s. FeatureSpace f s) -> SomeFeatureSpace f

instance (Aeson.ToJSON f, Ord f, Show f) => Aeson.ToJSON (SomeFeatureSpace f) where
    toJSON (SomeFeatureSpace fspace) = Aeson.toJSON fspace
    toEncoding (SomeFeatureSpace fspace) = Aeson.toEncoding fspace

instance (Aeson.FromJSON f, Ord f, Show f, Read f) => Aeson.FromJSON (SomeFeatureSpace f) where
    parseJSON v = do
      fs <- Aeson.parseJSON v
      return $ SomeFeatureSpace $ unsafeFromFeatureList $ V.toList fs

data FeatureSpace f s where
    -- | Space to create low level feature vectors
    Space :: { fsIndexToFeature :: VI.Vector V.Vector (FeatureIndex s) f
             , fsFeatureToIndex :: M.Map f (FeatureIndex s)
             }
          -> FeatureSpace f s
    deriving (Show)

instance (Aeson.ToJSON f, Ord f, Show f) => Aeson.ToJSON (FeatureSpace f s) where
    toJSON = Aeson.toJSON . featureNames
    toEncoding = Aeson.toEncoding . featureNames

data Stack (ss :: [Type])

mapFeatureNames :: Ord g => (f -> g) -> FeatureSpace f s -> FeatureSpace g s
mapFeatureNames f (Space a b) =
    Space { fsIndexToFeature = VI.map f a
          , fsFeatureToIndex = M.fromList $ fmap (first f) $ M.toList b
          }

eitherSpaces :: (Ord g, Ord f)
             => FeatureSpace f s -> FeatureSpace g s' -> FeatureSpace (Either f g) (Stack '[s, s'])
eitherSpaces s1 s2 = concatSpaces (mapFeatureNames Left s1) (mapFeatureNames Right s2)

concatSpaces :: Ord f => FeatureSpace f s -> FeatureSpace f s' -> FeatureSpace f (Stack '[s, s'])
concatSpaces s1 s2 =
    Space { fsIndexToFeature = VI.fromVector bnds
                               $ coerce (VI.vector $ fsIndexToFeature s1) <> coerce (VI.vector $ fsIndexToFeature s2)
          , fsFeatureToIndex = coerce (fsFeatureToIndex s1) <> fmap bump (fsFeatureToIndex s2)
          }
  where
    bump (FeatureIndex i) = FeatureIndex (i + dimension s1)
    dim = dimension s1 + dimension s2
    bnds = (FeatureIndex 0, FeatureIndex (dim - 1))

dimension :: FeatureSpace f s -> Int
dimension (Space v _) = rangeSize $ VI.bounds v

featureNames :: FeatureSpace f s -> [f]
featureNames = VI.elems . fsIndexToFeature

featureNameSet :: FeatureSpace f s -> S.Set f
featureNameSet = M.keysSet . fsFeatureToIndex

featureIndexBounds :: FeatureSpace f s -> (FeatureIndex s, FeatureIndex s)
featureIndexBounds = VI.bounds . fsIndexToFeature

featureIndexes :: FeatureSpace f s -> [FeatureIndex s]
featureIndexes = range . featureIndexBounds

-- | Lookup the feature index in fspace from name.
lookupFeatureIndex :: (Ord f, HasCallStack) => FeatureSpace f s -> f -> Maybe (FeatureIndex s)
lookupFeatureIndex (Space _ m) = (`M.lookup` m)
{-# INLINEABLE lookupFeatureIndex #-}

-- | Lookup the feature name from index.
lookupFeatureName :: HasCallStack => FeatureSpace f s -> FeatureIndex s -> f
lookupFeatureName (Space v _) = (v VI.!)
{-# INLINEABLE lookupFeatureName #-}

-- |  Intern a feature name.
internFeatureName :: Ord f => FeatureSpace f s -> f -> Maybe f
internFeatureName fspace fname = lookupFeatureName fspace <$> lookupFeatureIndex fspace fname
{-# INLINEABLE internFeatureName #-}

unsafeFromFeatureList :: (Ord f) => [f] -> FeatureSpace f s
unsafeFromFeatureList fnames = Space v m
  where
    m = M.fromList $ zip fnames (fmap FeatureIndex [0..])
    bs = (FeatureIndex 0, FeatureIndex $ M.size m - 1)
    v = VI.fromList bs $ fmap fst $ M.toAscList m

mkFeatureSpace :: (Ord f, HasCallStack)
               => S.Set f -> SomeFeatureSpace f
mkFeatureSpace fs = SomeFeatureSpace $ unsafeFromFeatureList $ S.toList fs

data FeatureVec f s a = FeatureVec { featureSpace  :: !(FeatureSpace f s)
                                   , getFeatureVec :: !(VI.Vector VU.Vector (FeatureIndex s) a)
                                   }
instance (Show f, Show a, VU.Unbox a) => Show (FeatureVec f s a) where
    show fvec =
        show $ toList fvec

instance NFData (FeatureVec f s a) where
    rnf x = x `seq` ()

-- | Vector addition.
instance (Num a, VU.Unbox a) => Semigroup (FeatureVec f s a) where
    (<>) = (^+^)

lookup :: (HasCallStack, VU.Unbox a, Ord f)
       => FeatureVec f s a -> f -> Maybe a
lookup v f = lookupIndex v <$> lookupFeatureIndex (featureSpace v) f
{-# INLINE lookup #-}

lookupIndex :: (HasCallStack, VU.Unbox a)
            => FeatureVec f s a -> FeatureIndex s -> a
lookupIndex (FeatureVec _ v) = (v VI.!)
{-# INLINE lookupIndex #-}

aggregateWith :: (Ord f, VU.Unbox a) => (a -> a -> a) -> NE.NonEmpty (FeatureVec f s a) -> FeatureVec f s a
aggregateWith f xs@(x NE.:| _) = FeatureVec (featureSpace x) $ VI.zipManyWith f (fmap getFeatureVec xs)

accum :: (Ord f, VU.Unbox a) => (a -> a -> a) -> FeatureVec f s a -> [(f, a)] -> FeatureVec f s a
accum f v = accumIndex f v . fmap (first $ fromMaybe err . lookupFeatureIndex (featureSpace v))
  where
    err = error "FeatureSpace.accum: Invalid feature name"

accumIndex :: VU.Unbox a => (a -> a -> a) -> FeatureVec f s a -> [(FeatureIndex s, a)] -> FeatureVec f s a
accumIndex f (FeatureVec space v) = FeatureVec space . VI.accum f v

modify :: (Ord f, VU.Unbox a) => FeatureVec f s a -> [(f, a)] -> FeatureVec f s a
modify = accum (const id)

-- | Create a 'FeatureVec' from a list of feature/value associations. Ignores
-- features not in the 'FeatureSpace'. Throws an error if some features are not
-- covered by the association list.
fromList :: (Show f, Ord f, VU.Unbox a)
         => FeatureSpace f s
         -> [(f, a)]
         -> FeatureVec f s a
fromList fspace xs = either err id $ fromList' fspace xs
  where
    err missingFeatures =
        error $ "SimplIR.FeatureSpace.fromList: Missing features: "++show missingFeatures

-- | Create a 'FeatureVec' from a list of feature/value associations. Ignores
-- features not in the 'FeatureSpace'.
fromList' :: (Ord f, VU.Unbox a)
          => FeatureSpace f s
          -> [(f, a)]
          -> Either (S.Set f) (FeatureVec f s a)
fromList' fspace xs = runST $ do
    acc <- VIM.new (featureIndexBounds fspace)
    flag <- VIM.replicate (featureIndexBounds fspace) False
    forM_ xs $ \(f, x) ->
        case lookupFeatureIndex fspace f of
          Just i -> do
            VIM.write acc i x
            VIM.write flag i True
          Nothing -> return ()

    flag' <- VI.unsafeFreeze flag
    if VU.and $ VI.vector flag'
        then Right . FeatureVec fspace <$> VI.unsafeFreeze acc
        else return $ Left $ S.fromList [ lookupFeatureName fspace fIdx | (fIdx, False) <- VI.assocs flag' ]

toList :: (VU.Unbox a) => FeatureVec f s a -> [(f, a)]
toList (FeatureVec fspace v) = zip (featureNames fspace) (VI.elems v)

toVector :: FeatureVec f s a -> VU.Vector a
toVector = VI.vector . getFeatureVec

-- | The only safe usage is to reconstitute a vector from the result of 'toVector':
--
-- >>> unsafeFromVector (featureSpace v) (toVector v) == v
--
unsafeFromVector :: (VU.Unbox a) => FeatureSpace f s -> VU.Vector a -> Maybe (FeatureVec f s a)
unsafeFromVector fspace v
  | dimension fspace == VU.length v =
      Just $ FeatureVec fspace $ VI.fromVector (featureIndexBounds fspace) v
  | otherwise = Nothing

repeat :: (VU.Unbox a) => FeatureSpace f s -> a -> FeatureVec f s a
repeat fspace x = FeatureVec fspace $ VI.replicate (featureIndexBounds fspace) x

generateM :: (VU.Unbox a, PrimMonad m) => FeatureSpace f s -> (f -> m a) -> m (FeatureVec f s a)
generateM fspace f = FeatureVec fspace <$> VI.generateM (featureIndexBounds fspace) (f . lookupFeatureName fspace)

generate :: VU.Unbox a => FeatureSpace f s -> (f -> a) -> FeatureVec f s a
generate fspace f = FeatureVec fspace $ VI.generate (featureIndexBounds fspace) (f . lookupFeatureName fspace)

data FeatureMapping f s g where
    FeatureMapping :: FeatureSpace g s'
                   -> (forall a. VU.Unbox a => FeatureVec f s a -> FeatureVec g s' a)
                   -> FeatureMapping f s g

mapFeatures :: forall f g s. (Show g, Ord g, Ord f)
            => FeatureSpace f s
            -> (f -> Maybe g)
            -> FeatureMapping f s g
mapFeatures srcSpace f =
    case mkFeatureSpace $ S.fromList $ fmap snd pairs of
      SomeFeatureSpace (destSpace :: FeatureSpace g s') ->
        let mapVec :: forall a. VU.Unbox a => FeatureVec f s a -> FeatureVec g s' a
            mapVec v = map (\i -> v `lookupIndex` FeatureIndex i) mapIdxs

            --mapIdxs :: FeatureVec g s' (FeatureIndex s)
            mapIdxs :: FeatureVec g s' Int
            mapIdxs = fromList destSpace [ (y, i)
                                         | (x, y) <- pairs
                                         , Just (FeatureIndex i) <- pure $ lookupFeatureIndex srcSpace x
                                         ]
        in FeatureMapping destSpace mapVec
  where
    pairs :: [(f, g)]
    pairs = mapMaybe (\x -> fmap (x,) (f x)) $ featureNames srcSpace

data FeatureMappingInto f s g s'
  where
    FeatureMappingInto :: (forall a. VU.Unbox a => FeatureVec f s a -> FeatureVec g s' a)
                       -> FeatureMappingInto f s g s'

mapFeaturesInto :: forall f g s s'. (Show g, Ord g, Ord f)
                => FeatureSpace f s
                -> FeatureSpace g s'
                -> (f -> Maybe g)
                -> Maybe (FeatureMappingInto f s g s')
mapFeaturesInto srcSpace destSpace featProj
  | missingFeaturesInSourceSpace <- featureNameSet destSpace `S.difference` S.fromList (fmap snd pairs)
  , S.null missingFeaturesInSourceSpace =
      let mapVec :: forall a. VU.Unbox a => FeatureVec f s a -> FeatureVec g s' a
          mapVec v = map (\i -> v `lookupIndex` FeatureIndex i) mapIdxs

          --mapIdxs :: FeatureVec g s' (FeatureIndex s)
          mapIdxs :: FeatureVec g s' Int
          mapIdxs = fromList destSpace [ (y, i)
                                        | (x, y) <- pairs
                                        , Just (FeatureIndex i) <- pure $ lookupFeatureIndex srcSpace x
                                        ]
      in Just $ FeatureMappingInto mapVec
  | otherwise = Nothing
  where
    pairs :: [(f, g)]
    pairs = mapMaybe (\x -> fmap (x,) (featProj x)) $ featureNames srcSpace

map :: (VU.Unbox a, VU.Unbox b)
    => (a -> b) -> FeatureVec f s a -> FeatureVec f s b
map f (FeatureVec fspace x) = FeatureVec fspace $ VI.map f x
{-# INLINE map #-}

-- | @project s1 s2@ returns a constructive proof that vectors of one feature
-- space @s1@ can be projected into @s2@.
project :: forall m f s s' a. (VU.Unbox a, Ord f, MonadFail m)
        => FeatureSpace f s -> FeatureSpace f s'
        -> m (FeatureVec f s a -> FeatureVec f s' a)
project fs1@(Space _ s1) fs2
  | Just f <- fs1 `equivSpace` fs2 = return f
  | otherwise = case mapping of
                  Right x  -> pure $ \v -> map (lookupIndex v . coerce) x
                  Left err -> fail err
  where
    --mapping :: Either String (FeatureVec f s' (FeatureIndex s))
    mapping :: Either String (FeatureVec f s' Int)
    !mapping = runST $ runExceptT $ generateM fs2 $ \f ->
      case lookupFeatureIndex fs1 f of
        Just (FeatureIndex i) -> pure i
        Nothing -> throwE $ "project: Feature not present"

data ProjectBothResult f s1 s2 where
    ProjectBothResult :: { pbrFeatureSpace :: FeatureSpace s f
                         , pbrFst          :: forall a. VU.Unbox a => FeatureVec f s1 a -> FeatureVec f s a
                         , pbrSnd          :: forall a. VU.Unbox a => FeatureVec f s2 a -> FeatureVec f s a
                         } -> ProjectBothResult f s1 s2

-- | @projectBoth s1 s2@ returns a constructive proof that vectors in two vector
-- spaces can be projected to the intersection of their dimensions.
projectBoth :: forall f s1 s2. (Ord f)
            => FeatureSpace f s1 -> FeatureSpace f s2
            -> ProjectBothResult f s1 s2
projectBoth s1 s2 =
  let feats = featureNameSet s1 `S.intersection` featureNameSet s2
      SomeFeatureSpace s = mkFeatureSpace feats
      proj a b = fromMaybe (error "projectBoth: impossible") $ project a b
  in ProjectBothResult s (proj s1 s) (proj s2 s)

-- | Returns a constructive proof that two feature spaces have the same features
-- in the same order.
equivSpace :: forall f s s' a. (VU.Unbox a, Ord f)
           => FeatureSpace f s -> FeatureSpace f s'
           -> Maybe (FeatureVec f s a -> FeatureVec f s' a)
equivSpace fs1@(Space s1 _) fs2@(Space s2 _)
  | Just Refl <- fs1 `eqSpace` fs2 = Just $ FeatureVec fs2 . getFeatureVec
  | featureNameSet fs1 == featureNameSet fs2
  = let mapping :: FeatureVec f s' Int
        !mapping = runST $ generateM fs2 $ \f ->
          case lookupFeatureIndex fs1 f of
            Just (FeatureIndex i) -> pure i
            Nothing -> error "equivSpace: Impossibl"
    in Just $ \v -> map (lookupIndex v . coerce) mapping
  | otherwise = Nothing

eqSpace :: forall f s s'. (Eq f)
        => FeatureSpace f s -> FeatureSpace f s'
        -> Maybe (s :~: s')
eqSpace (Space s1 _) fs2@(Space s2 _)
  | VI.vector s1 == VI.vector s2  = Just (unsafeCoerce Refl)
  | otherwise = Nothing

data FeatureStack f ss a where
    Stack :: FeatureVec f s a -> FeatureStack f ss a -> FeatureStack f (s ': ss) a
    StackNil :: FeatureStack f '[] a

infixr 5 `Stack`

stack :: forall f ss a. (Ord f, VU.Unbox a)
      => FeatureSpace f (Stack ss) -> FeatureStack f ss a -> FeatureVec f (Stack ss) a
stack fspace vecs =
    FeatureVec fspace $ VI.fromVector (featureIndexBounds fspace) (VU.concat $ featVecs vecs)
  where
    featVecs :: forall ss'. FeatureStack f ss' a -> [VU.Vector a]
    featVecs (Stack f rest) = VI.vector (getFeatureVec f) : featVecs rest
    featVecs StackNil = []

zipWith :: (VU.Unbox a, VU.Unbox b, VU.Unbox c)
        => (a -> b -> c) -> FeatureVec f s a -> FeatureVec f s b -> FeatureVec f s c
zipWith f v1 v2 =
    FeatureVec (featureSpace v1) $ VI.zipWith f (getFeatureVec v1) (getFeatureVec v2)
{-# INLINE zipWith #-}

scale :: (VU.Unbox a, Num a)
      => a -> FeatureVec f s a -> FeatureVec f s a
scale x = map (*x)
{-# SPECIALISE scale :: Double -> FeatureVec f s Double -> FeatureVec f s Double #-}

(^+^), (^-^), (^*^)
    :: (VU.Unbox a, Num a)
    => FeatureVec f s a
    -> FeatureVec f s a
    -> FeatureVec f s a
(^+^) = zipWith (+)
(^-^) = zipWith (-)
(^*^) = zipWith (*)

(^/^) :: (VU.Unbox a, RealFrac a)
      => FeatureVec f s a
      -> FeatureVec f s a
      -> FeatureVec f s a
(^/^) = zipWith (/)
{-# INLINEABLE (^/^) #-}
{-# INLINEABLE (^*^) #-}
{-# INLINEABLE (^+^) #-}
{-# INLINEABLE (^-^) #-}

sum :: (VU.Unbox a, Num a) => FeatureVec f s a -> a
sum = VI.sum . getFeatureVec
--{-# INLINE sum #-}

dot :: (VU.Unbox a, Num a) => FeatureVec f s a -> FeatureVec f s a -> a
-- We would like to write the following but sadly GHC can't be convinced to fuse
-- sum and zipWith:
-- dot v0 v1 = sum (v0 ^*^ v1)
dot v1 v2 = VI.dot (getFeatureVec v1) (getFeatureVec v2)
{-# SPECIALISE dot :: FeatureVec f s Double -> FeatureVec f s Double -> Double #-}
{-# SPECIALISE dot :: FeatureVec f s Float -> FeatureVec f s Float -> Float #-}

quadrance :: (VU.Unbox a, Num a) => FeatureVec f s a -> a
-- We would like to write the following but sadly GHC can't be convinced to fuse
-- sum and map:
--quadrance = VI.sum . VI.map (\x -> x*x) . getFeatureVec
quadrance = VI.quadrance . getFeatureVec
{-# SPECIALISE quadrance :: FeatureVec f s Double -> Double #-}
{-# SPECIALISE quadrance :: FeatureVec f s Float -> Float #-}

l2Norm :: (VU.Unbox a, RealFloat a) => FeatureVec f s a -> a
l2Norm = sqrt . quadrance
{-# INLINE l2Norm #-}

-- | Update the values at the given 'FeatureIndex's.
modifyIndices :: VU.Unbox a => FeatureVec f s a -> [(FeatureIndex s, a)] -> FeatureVec f s a
FeatureVec space v `modifyIndices` xs = FeatureVec space (v VI.// coerce xs)
{-# INLINE modifyIndices #-}

-- | 'F.FoldM' for bulk creation of 'FeatureVec's.
mkFeaturesF :: forall f s a m i. (PrimMonad m, Ord f, Ord i, VU.Unbox a)
           => FeatureSpace f s
           -> (f -> a)         -- ^ default feature value
           -> (a -> a -> a)    -- ^ accumulation function
           -> F.FoldM m (i, f, a) (M.Map i (FeatureVec f s a))
-- mkFeatures :: FeatureSpace feature s
--             -> S.Set node   -- set that will get feature vectors
--             -> (value -> value -> value)
--             -> [(node, feature, value)]
--             -> M.Map node (FeatureVec feature s value)
mkFeaturesF fspace def plus = F.FoldM step initial finish
  where
    mkEmptyVec :: m (RawFeatureVecM m s a, RawFeatureVecM m s Bool)
    mkEmptyVec = do
        vals <- VIM.new (featureIndexBounds fspace)
        sets <- VIM.replicate (featureIndexBounds fspace) False
        return (vals, sets)

    initial = pure mempty

    step :: M.Map i (RawFeatureVecM m s a, RawFeatureVecM m s Bool)
         -> (i, f, a)
         -> m (M.Map i (RawFeatureVecM m s a, RawFeatureVecM m s Bool))
    step acc (x, f, v)
      | Just fIdx <- lookupFeatureIndex fspace f = do
            (accum', (fVec, setVec)) <-
                case M.lookup x acc of
                  Just y   -> return (acc, y)
                  Nothing  -> do pair <- mkEmptyVec
                                 return (M.insert x pair acc, pair)

            isFeatureSet <- VIM.read setVec fIdx
            v' <- if isFeatureSet
                  then do v0 <- VIM.read fVec fIdx
                          return $! v0 `plus` v
                  else return v

            VIM.write fVec fIdx v'
            VIM.write setVec fIdx True
            return accum'

      | otherwise = return acc

    finish :: M.Map i (RawFeatureVecM m s a, RawFeatureVecM m s Bool)
           -> m (M.Map i (FeatureVec f s a))
    finish acc =
        forM acc $ \(fVec, setVec) -> do
            setVec' <- VI.unsafeFreeze setVec
            VI.imapM_ (setUnsetFeatures fVec) setVec'
            FeatureVec fspace <$> VI.unsafeFreeze fVec
      where
        setUnsetFeatures :: RawFeatureVecM m s a -> FeatureIndex s -> Bool -> m ()
        setUnsetFeatures _fVec _fIdx True  = return ()
        setUnsetFeatures fVec  fIdx  False =
            VIM.write fVec fIdx $ def (lookupFeatureName fspace fIdx)

type RawFeatureVecM m s = VIM.MVector VUM.MVector (PrimState m) (FeatureIndex s)
