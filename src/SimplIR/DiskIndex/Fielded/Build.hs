{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
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

module SimplIR.DiskIndex.Fielded.Build
    ( buildOneIndex
    , buildIndex
    ) where

import qualified Control.Foldl as Foldl
import Control.Monad.IO.Class
import qualified Data.Map as M
import Data.Semigroup
import Data.Binary (Binary)
import Data.Kind
import Data.Vinyl
import Data.Vinyl.Functor
import Data.Vinyl.TypeLevel
import Data.Profunctor hiding (rmap)
import Data.Bifunctor
import Data.Coerce
import Data.Singletons.TH

import Pipes.Safe (MonadSafe, ReleaseKey)
import qualified Pipes.Safe as Safe
import System.FilePath ((</>))
import System.IO (hClose)
import System.IO.Temp
import System.Directory (removeFile)

import SimplIR.Types (DocumentId, Posting(..))
import SimplIR.Term (Term)
import SimplIR.Utils
import qualified SimplIR.DiskIndex.Posting as Postings
import qualified SimplIR.DiskIndex.Document as DocMeta
import SimplIR.DiskIndex.Fielded.Types

import SimplIR.Types (Position)

-- Test
data Field = ABoolField | APositionalField
genSingletons [''Field]

type family FieldPosting (a :: Field) :: * where
    FieldPosting 'ABoolField = ()
    FieldPosting 'APositionalField = [Position]

newtype Attr f = Attr { unAttr :: FieldPosting f }
instance Show (Attr 'ABoolField) where show (Attr x) = show x
instance Show (Attr 'APositionalField) where show (Attr x) = show x

(=::) :: sing f -> FieldPosting f -> Attr f
_ =:: x = Attr x

type Doc = Rec Attr '[ 'ABoolField, 'APositionalField ]

aDoc :: Doc
aDoc = (SABoolField =:: ()) :& (SAPositionalField =:: []) :& RNil

-- Single-fielded index
data DefaultField = DefaultField
type family SingleField p (a :: DefaultField) :: Type where
    SingleField p 'DefaultField = p
newtype SingleAttr p f = SingleAttr { unSingleAttr :: SingleField p f }

deriving instance Binary (SingleField p f) => Binary (SingleAttr p f)


-- Build
instance NamedFields DefaultField where
    fieldName Proxy = "default"

-- | Build a single non-fielded index.
buildOneIndex
    :: forall m docmeta p. (MonadSafe m, Binary docmeta, Binary p)
    => Int       -- ^ How many documents to include in each index chunk?
    -> String    -- ^ Final index path
    -> Foldl.FoldM m (docmeta, M.Map Term p)
                     (DocMeta.OnDiskIndex docmeta, Postings.OnDiskIndex p)
buildOneIndex chunkSize outputPath =
      dimap (second $ \x -> Compose (coerce x) :& RNil)
            (second $ \(Compose x :& RNil) -> coerce x)
    $ buildIndex @m @docmeta @DefaultField @('[ 'DefaultField ]) @(SingleAttr p)
                 chunkSize outputPath

-- | Build a set of indexes, one for each document field.
buildIndex
    :: forall m docmeta
              field (fields :: [field])
              (postingType :: field -> Type).
       (MonadSafe m, Binary docmeta,
        RecAll postingType fields Binary, RecApplicative fields,
        NamedFields field)
    => Int       -- ^ How many documents to include in each index chunk?
    -> FilePath  -- ^ Final index path
    -> Foldl.FoldM m (docmeta, Rec (Compose (M.Map Term) postingType) fields)
                     ( DocMeta.OnDiskIndex docmeta
                     , Rec (Compose Postings.OnDiskIndex postingType) fields )
buildIndex chunkSize outputPath =
      zipFoldM (toEnum 0 :: DocumentId) succ
    $ foldChunksOf chunkSize (Foldl.generalize buildDocMetaIndex)
                             ((,) <$> Foldl.premapM fst (reduceDocMetaChunk docIdxPath)
                                  <*> Foldl.premapM snd (reducePostingChunks postingPaths))
  where
    onDisk = OnDiskIndex outputPath
    docIdxPath = DocMeta.getOnDiskPath $ getDocumentIndexPath onDisk

    postingPaths :: Rec (Const FilePath) fields
    postingPaths = rpure fieldName'

    fieldName' :: forall f. Const FilePath (f :: field)
    fieldName' = Const $ Postings.getOnDiskPath $ getFieldIndexPath onDisk (Proxy :: Proxy f)

-- | A chunk of postings.
newtype IndexChunk (postingType :: field -> Type) (a :: field)
    = IndexChunk (M.Map Term [Posting (postingType a)])

instance Monoid (IndexChunk postingType a) where
    mempty = IndexChunk mempty
    mappend = (<>)
instance Semigroup (IndexChunk postingType a) where
    IndexChunk a <> IndexChunk b =
        IndexChunk $ M.unionWith (++) a b

buildDocMetaIndex
    :: forall docmeta field (fields :: [field]) postingType.
       (RecApplicative fields)
    => Foldl.Fold (DocumentId, (docmeta, Rec (Compose (M.Map Term) postingType) fields))
                  (M.Map DocumentId docmeta, Rec (IndexChunk postingType) fields)
buildDocMetaIndex = Foldl.Fold step initial finish
  where
    initial :: (M.Map DocumentId docmeta, Rec (IndexChunk postingType) fields)
    initial = (mempty, rpure mempty)
    finish = id
    step (docMetaChunk, postingsChunk) (docId, (docMeta, fieldPostings)) =
        let docMetaChunk' = M.insert docId docMeta docMetaChunk

            toPosting :: Compose (M.Map Term) postingType a -> IndexChunk postingType a
            toPosting (Compose postings) = IndexChunk $ fmap (\p -> [Posting docId p]) postings

            postingsChunk' :: Rec (IndexChunk postingType) fields
            postingsChunk' = Lift . mappend <<$>> postingsChunk <<*>> rmap toPosting fieldPostings
        in (docMetaChunk', postingsChunk')

createTempFile :: (MonadSafe m)
               => FilePath -> String -> m (FilePath, ReleaseKey)
createTempFile dir name = do
    (path, h) <- liftIO $ openTempFile dir name
    liftIO $ hClose h
    key <- Safe.register $ liftIO $ removeFile path
    return (path, key)

reduceDocMetaChunk
    :: forall docmeta m. (MonadSafe m, Binary docmeta)
    => FilePath
    -> Foldl.FoldM m (M.Map DocumentId docmeta) (DocMeta.OnDiskIndex docmeta)
reduceDocMetaChunk outputPath = postmapM' finalMerge $ Foldl.sink $ \docs -> do
    (path, key) <- createTempFile "." "part.index"
    let indexPath = DocMeta.OnDiskIndex path
    liftIO $ DocMeta.write indexPath docs
    return [(indexPath, key)]
  where
    finalMerge :: [(DocMeta.OnDiskIndex docmeta, ReleaseKey)]
               -> m (DocMeta.OnDiskIndex docmeta)
    finalMerge chunkIndexes = do
        idxs <- liftIO $ mapM (DocMeta.open . fst) chunkIndexes
        let indexPath = DocMeta.OnDiskIndex outputPath
        -- We needn't use the DocIdDeltas since we know that all of the
        -- documents have unique document IDs
        _ <- liftIO $ DocMeta.merge indexPath idxs
        mapM_ (Safe.release . snd) chunkIndexes
        return indexPath

reducePostingChunk
    :: forall m field (postingType :: field -> Type) (f :: field).
       (MonadSafe m, Binary (postingType f))
    => FilePath
    -> Foldl.FoldM m (IndexChunk postingType f) (Postings.OnDiskIndex (postingType f))
reducePostingChunk outputPath = postmapM' finalMerge $ Foldl.sink $ \(IndexChunk postings) -> do
    (path, key) <- createTempFile "." "part.index"
    let indexPath = Postings.OnDiskIndex path
    liftIO $ Postings.fromTermPostings 128 indexPath postings
    return [(indexPath, key)]
  where
    finalMerge :: forall p. Binary p
               => [(Postings.OnDiskIndex p, ReleaseKey)]
               -> m (Postings.OnDiskIndex p)
    finalMerge chunkIndexes = do
        let indexPath = Postings.OnDiskIndex outputPath
        liftIO $ Postings.merge indexPath
               $ zip (repeat $ toEnum 0) (map fst chunkIndexes)
        mapM_ (Safe.release . snd) chunkIndexes
        return indexPath

reducePostingChunks
    :: forall field (fields :: [field]) postingType m.
       (MonadSafe m, RecAll postingType fields Binary)
    => Rec (Const FilePath) fields
    -> Foldl.FoldM m (Rec (IndexChunk postingType) fields)
                     (Rec (Compose Postings.OnDiskIndex postingType) fields)
reducePostingChunks outputPaths =
    rhandlesM reducers
  where
    reducers :: Rec (Handler m (IndexChunk postingType)
                               (Compose Postings.OnDiskIndex postingType)) fields
    reducers = rmapC (Proxy @Binary) (Proxy @postingType) f outputPaths

    f :: forall (x :: field). Binary (postingType x)
      => Const FilePath x
      -> Handler m (IndexChunk postingType) (Compose Postings.OnDiskIndex postingType) x
    f = Handler . fmap Compose . reducePostingChunk . getConst

type family CAll (f :: a -> Constraint) (xs :: [a]) :: Constraint where
    CAll f (x ': xs) = (f x, CAll f xs)
    CAll _ '[]       = ()

rmapC :: forall field (c :: Type -> Constraint) (xs :: [field])
                (f :: field -> Type) (g :: field -> Type) (h :: field -> Type). (RecAll h xs c)
      => Proxy c -> Proxy h
      -> (forall (x :: field). c (h x) => f x -> g x)
      -> Rec f xs -> Rec g xs
rmapC proxyC proxyH f (r :& rs) = f r :& rmapC proxyC proxyH f rs
rmapC _ _ _ RNil = RNil

newtype Handler m src dest f = Handler (Foldl.FoldM m (src f) (dest f))

collectHandler :: Monad m
               => Handler m src dest (g f)
               -> Handler m (Compose src g) (Compose dest g) f
collectHandler (Handler f) = Handler $ dimap getCompose Compose f

distributeHandler :: Monad m
                  => Handler m (Compose src g) (Compose dest g) f
                  -> Handler m src dest (g f)
distributeHandler (Handler f) = Handler $ dimap Compose getCompose f

-- | Use a set of 'FoldM's to fold over the various pieces of a record.
rhandlesM :: (Monad m)
          => Rec (Handler m f g) fields
          -> Foldl.FoldM m (Rec f fields) (Rec g fields)
rhandlesM (Handler h :& hs) =
    (:&) <$> Foldl.premapM (\(x :& _) -> x) h <*> Foldl.premapM (\(_ :& rest) -> rest) (rhandlesM hs)
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
