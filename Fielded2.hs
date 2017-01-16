{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module FieldedIndex where

import qualified Control.Foldl as Foldl
import Control.Monad.IO.Class
import qualified Data.Map as M
import Data.Binary (Binary)
import Data.Kind
import Data.Vinyl
import Data.Vinyl.Functor
import Data.Vinyl.TypeLevel
import Data.Profunctor hiding (rmap)
import Data.Bitraversable
import Data.Bifunctor
import Data.Coerce
import qualified Data.Promotion.Prelude as P
import Data.Singletons.TH

newtype Window = Window Int
               deriving (Enum, Show, Eq, Ord)

newtype Position = Position Int
               deriving (Enum, Show, Eq, Ord)


-- Appklication
data Field = ABoolField | APositionalField
genSingletons [''Field]

type family FieldPosting (a :: Field) :: * where
    FieldPosting 'ABoolField = ()
    FieldPosting 'APositionalField = [Position]

newtype Attr f = Attr { unAttr :: FieldPosting f }
instance Show (Attr ABoolField) where show (Attr x) = show x
instance Show (Attr APositionalField) where show (Attr x) = show x

(=::) :: sing f -> FieldPosting f -> Attr f
_ =:: x = Attr x

type Doc = Rec Attr '[ 'ABoolField, 'APositionalField ]

aDoc :: Doc
aDoc = (SABoolField =:: ()) :& (SAPositionalField =:: []) :& RNil

-- Single-fielded index
type family SingleField p (a :: ()) :: * where
    SingleField p '() = p
newtype SingleAttr p f = SingleAttr { unSingleAttr :: SingleField p f }

deriving instance Binary (SingleField p f) => Binary (SingleAttr p f)


-- Build
class MonadIO m => MonadSafe m
data Term = Term String deriving (Eq, Ord, Show)
newtype DocumentId = DocumentId Int deriving (Eq, Ord, Show, Enum)
data OnDiskPostingIndex p
data OnDiskDocMetaIndex docmeta
newtype FieldedIndex docmeta field (fields :: [field]) (postingType :: field -> Type) = FieldedIndex ()
data Posting p = Posting DocumentId p

foldChunksOf :: Monad m
             => Int                 -- ^ Chunk size
             -> Foldl.FoldM m a b   -- ^ "inner" fold, reducing "points"
             -> Foldl.FoldM m b c   -- ^ "outer" fold, reducing chunks
             -> Foldl.FoldM m a c
foldChunksOf = undefined

-- | Build a single non-fielded index.
buildOneIndex
    :: forall m docmeta p. (MonadSafe m, Binary docmeta, Binary p)
    => Int       -- ^ How many documents to include in each index chunk?
    -> String    -- ^ Final index path
    -> Foldl.FoldM m (docmeta, M.Map Term p)
                     (OnDiskDocMetaIndex docmeta, OnDiskPostingIndex p)
buildOneIndex chunkSize outputPath =
      dimap (second $ \x -> Compose (coerce x) :& RNil)
            (second $ \(Compose x :& RNil) -> coerce x)
    $ buildIndex @m @docmeta @() @('[ '() ]) @(SingleAttr p)
                 chunkSize (Const outputPath :& RNil)

-- | Build a set of indexes, one for each document field.
buildIndex
    :: forall m docmeta
              field (fields :: [field])
              (postingType :: field -> Type).
       (MonadSafe m, Binary docmeta,
        RecAll postingType fields Binary, RecApplicative fields, CanRHandle fields)
    => Int       -- ^ How many documents to include in each index chunk?
    -> Rec (Const String) fields  -- ^ Final index path
    -> Foldl.FoldM m (docmeta, Rec (Compose (M.Map Term) postingType) fields)
                     ( OnDiskDocMetaIndex docmeta
                     , Rec (Compose OnDiskPostingIndex postingType) fields )
buildIndex chunkSize outputPaths =
    zipFoldM (DocumentId 0) succ
    $ foldChunksOf chunkSize (Foldl.generalize buildDocMetaIndex)
                             ((,) <$> Foldl.premapM fst writeDocMetaChunk
                                  <*> Foldl.premapM snd writePostingChunks)

-- | A chunk of postings.
newtype IndexChunk (postingType :: field -> Type) (a :: field)
    = IndexChunk (M.Map Term (Posting (postingType a)))
    deriving (Monoid)

buildDocMetaIndex
    :: forall docmeta field (fields :: [field]) postingType.
       (RecApplicative fields)
    => Foldl.Fold (DocumentId, (docmeta, Rec (Compose (M.Map Term) postingType) fields))
                  ([(DocumentId, docmeta)], Rec (IndexChunk postingType) fields)
buildDocMetaIndex = Foldl.Fold step initial finish
  where
    initial :: ([(DocumentId, docmeta)], Rec (IndexChunk postingType) fields)
    initial = ([], rpure mempty)
    finish = id
    step (docMetaChunk, postingsChunk) (docId, (docMeta, fieldPostings)) =
        let docMetaChunk' = (docId, docMeta) : docMetaChunk

            toPosting :: Compose (M.Map Term) postingType a -> IndexChunk postingType a
            toPosting (Compose postings) = IndexChunk $ fmap (Posting docId) postings

            postingsChunk' :: Rec (IndexChunk postingType) fields
            postingsChunk' = Lift . mappend <<$>> postingsChunk <<*>> rmap toPosting fieldPostings
        in (docMetaChunk', postingsChunk')

writeDocMetaChunk
    :: forall docmeta m. (MonadIO m)
    => Foldl.FoldM m [(DocumentId, docmeta)] (OnDiskDocMetaIndex docmeta)
writeDocMetaChunk = undefined

writePostingChunk
    :: forall m field (postingType :: field -> Type) (f :: field).
       (MonadIO m)
    => Foldl.FoldM m (IndexChunk postingType f) (OnDiskPostingIndex (postingType f))
writePostingChunk = undefined

writePostingChunks
    :: forall field (fields :: [field]) postingType m.
       (MonadIO m, RecApplicative fields, CanRHandle fields)
    => Foldl.FoldM m (Rec (IndexChunk postingType) fields)
                     (Rec (Compose OnDiskPostingIndex postingType) fields)
writePostingChunks =
    rhandlesM (rpure $ Handler $ fmap Compose writePostingChunk)

newtype Handler m src dest f = Handler (Foldl.FoldM m (src f) (dest f))

class CanRHandle (fields :: [a]) where
    -- | Use a set of 'FoldM's to fold over the various pieces of a record.
    rhandlesM :: (Monad m)
              => Rec (Handler m f g) fields
              -> Foldl.FoldM m (Rec f fields) (Rec g fields)
instance (CanRHandle fs) => CanRHandle (f ': fs) where
    rhandlesM (Handler h :& hs) =
        (:&) <$> Foldl.premapM (\(x :& _) -> x) h <*> Foldl.premapM (\(_ :& rest) -> rest) (rhandlesM hs)
instance CanRHandle '[] where
    rhandlesM RNil = pure RNil

zipFoldM :: forall i m a b. Monad m
         => i -> (i -> i)
         -> Foldl.FoldM m (i, a) b
         -> Foldl.FoldM m a b
zipFoldM idx0 succ' (Foldl.FoldM step0 initial0 extract0) =
    Foldl.FoldM step initial extract
  where
    initial = do s <- initial0
                 return (idx0, s)
    extract = extract0 . snd
    step (!idx, s) x = do
        s' <- step0 s (idx, x)
        return (succ' idx, s')
{-# INLINEABLE zipFoldM #-}
