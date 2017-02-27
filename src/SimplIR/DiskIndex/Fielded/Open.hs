{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module SimplIR.DiskIndex.Fielded.Open where

import Control.Monad.IO.Class
import Data.Coerce
import Data.Kind
import Data.Vinyl
import Data.Vinyl.Functor
import Data.Vinyl.TypeLevel (RecAll, RImage)
import Data.Singletons.TH
import Pipes hiding (Proxy)
import SimplIR.Term
import Control.Monad.Except

import qualified SimplIR.DiskIndex.Posting as Postings
import qualified SimplIR.DiskIndex.Document as DocMeta
import SimplIR.DiskIndex.Fielded.Types

data FieldedIndex docmeta (fields :: [field]) (postingType :: field -> Type)
    = FieldedIndex { docMetaIndex :: DocMeta.DocIndex docmeta
                   , fieldIndexes :: Rec (Compose Postings.DiskIndex postingType) fields
                   }

open :: forall docmeta field (fields :: [field]) (postingType :: field -> Type).
        (NamedFields field, RecApplicative fields)
     => OnDiskIndex docmeta fields postingType
     -> ExceptT String IO (FieldedIndex docmeta fields postingType)
open idx = do
    let openIndex :: forall (f :: field). ()
                  => Proxy f
                  -> ExceptT String IO (Compose Postings.DiskIndex postingType f)
        openIndex = fmap Compose . ExceptT . Postings.open . getFieldIndexPath idx

    idxs <- rtraverse openIndex (rproxies (Proxy @fields))
    docMeta <- liftIO $ DocMeta.open $ getDocumentIndexPath idx
    return (FieldedIndex docMeta idxs)

rproxies :: forall fields. RecApplicative fields
         => Proxy fields -> Rec Proxy (fields :: [field])
rproxies _ = rpure Proxy

testOpen :: ExceptT String IO (FieldedIndex Char '[ 'DefaultField ] (SingleAttr Int))
testOpen = open (OnDiskIndex "test")

-- | 'Producer' with reordered type arguments.
newtype RProducer m a = Producer' (Producer a m ())

data WithKey a b = WithKey !a !b
                 deriving (Show, Functor)
type PostingProducer m p = Compose (RProducer m) (PostingList p)
newtype PostingList p f = PostingList [(Term, p f)]

data ZipState m n p f = ZipState { zipMinAfter :: !(Maybe n)
                                 , zipProducer :: Maybe (WithKey n (p f), Producer (p f) m ())
                                 }

{-
rzipSorted :: forall p fields m n. (Ord n)
           => Rec (Producer' m :. WithKey n :. p) fields
           -> Producer (Rec (Maybe :. WithKey n :. p) fields) m ()
rzipSorted = initialize >=> start
  where
    initialize :: Rec (Producer' m :. WithKey n :. p) fields'
               -> m (Maybe n, Rec (ZipState m n p) fields)
    initialize RNil = (Nothing, RNil)
    initialize (prod :& rest) = do
        mr <- next prod
        case mr of
          Nothing -> do
              (n, rest') <- initialize rest
              return (n, ZipState n Nothing :& rest')
          Just (WithKey n0 x, prod') -> do
              (n, rest') <- initialize rest
              let minAfter = maybe n0 (min n0) n
              return (minAfter, ZipState n (WithKey n0 x, prod'))


    start :: Rec (ZipState m n p) fields'
          -> Producer (Rec (Maybe :. WithKey n :. p) fields) m ()
    start s = go s (rpure Nothing)

    go :: Rec (ZipState m n p) fields'
       -> Rec (Maybe :. WithKey n :. p) fields
       -> Producer (Rec (Maybe :. WithKey n :. p) fields) m
                   (Maybe n, Rec (ZipState m n p) fields')
    go (ZipState {..} :& rest) acc
      | Just (WithKey n0 x0, prod) <- zipProducer
      , Just n <- zipMinAfter
      , n0 == n
      = do (nDown, rest') <- go rest (rput (WithKey n0 x0) acc)
           mr <- next prod
           case mr of
             Just (WithKey n1 x1, prod') ->
                 let minAfter' = maybe nDown
                 return $ ZipState (min `h` hi)
             Nothing ->
                 return $ ()

      | otherwise
      = go 
-}

data Hi (fields :: [field]) (p :: field -> Type) where
    Hi :: forall fields p x. (SingI x)
       => (forall p. p x -> Rec p fields -> Rec p fields)
       -> p x
       -> Hi fields p

fromHi :: forall fields p. (RecApplicative fields)
       => [Hi fields p] -> Rec (Maybe `Compose` p) fields
fromHi = go (rpure $ Compose Nothing :: Rec (Maybe `Compose` p) fields)
  where
    go :: Rec (Maybe `Compose` p) fields
       -> [Hi fields p]
       -> Rec (Maybe `Compose` p) fields
    go acc (Hi put (x :: p x) : rest) = go (put (Compose $ Just x) acc) rest
    go acc [] = acc

class Grow (rs :: [field]) (ss :: [field]) where
    -- | Widen the domain of a thing
    grow :: Hi rs f -> Hi ss f

instance Grow rs (s ': rs) where
    grow (Hi put xs) = Hi (\x (y :& rest) -> y :& put x rest) xs
instance Grow rs rs where
    grow = id

{-
data GrowEv (rs :: [field]) (ss :: [field]) where
    Same  :: GrowEv rs rs
    There :: GrowEv rs ss -> GrowEv rs (s ': ss)

type family ToGrowEv (rs :: [field]) (ss :: [field]) where
    ToGrowEv rs  rs        = 'Same
    ToGrowEv rs  (s ': ss) = 'There (ToGrowEv rs ss)

class Grow' field (rs::[field]) (ss::[field]) (ev :: GrowEv rs ss) where
    grow :: Proxy ev -> Hi rs p -> Hi ss p

instance Grow' field ss ss 'Same where
    grow Proxy (Hi put xs) = Hi put xs

instance Grow' field rs (s ': rs) 'There where
    grow Proxy (Hi put xs) =
        Hi (\x (r :& rs) -> r :& put x rs) xs
-}

toHi :: forall field (p :: field -> Type) (fields :: [field]). (CAll SingI fields)
     => Rec p fields -> [Hi fields p]
toHi = go
  where
    go :: forall fields'. (Grow fields' fields)
       => Rec p fields' -> [Hi fields p]
    go (x :& rest) = grow hi : go rest
      where
        hi :: Hi fields' p
        hi = Hi rput x
    go RNil = []

type family CAll (f :: a -> Constraint) (xs :: [a]) :: Constraint where
    CAll f (x ': xs) = (f x, CAll f xs)
    CAll _ '[]       = ()

--rzipSorted :: forall p fields m n. (Ord n)
--           => Rec (Producer' m :. WithKey n :. p) fields
--           -> Producer (Rec (Maybe :. WithKey n :. p) fields) m ()
--rzipSorted = undefined
