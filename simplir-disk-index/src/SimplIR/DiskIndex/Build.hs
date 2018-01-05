{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}

-- | Build disk indexes in a memory-friendly manner.
module SimplIR.DiskIndex.Build
    ( buildIndex
    ) where

import Control.Concurrent
import Control.Concurrent.Map
import Control.Concurrent.Async.Lifted
import Control.Monad.IO.Class
import Data.Profunctor
import System.Directory
import System.IO.Temp
import Data.List.Split

import qualified Data.Map.Strict as M
import qualified Control.Foldl as Foldl
import Data.Binary
import Pipes.Safe

import qualified SimplIR.DiskIndex as DiskIdx
import SimplIR.Types
import Control.Foldl.Map
import SimplIR.Utils

-- Using m ~ SafeT IO until we are certain that other monads won't be needed.

buildIndex :: forall term m doc p.
              (m ~ SafeT IO, Binary term, Ord term, Binary doc, Binary p)
           => Int       -- ^ How many documents to include in an index chunk?
           -> FilePath  -- ^ Final index path
           -> Foldl.FoldM m (doc, M.Map term p) (DiskIdx.OnDiskIndex term doc p)
buildIndex chunkSize outputPath =
    postmapM' moveResult
    $ postmapM' (treeReduce 256 mergeIndexChunks)
    $ foldChunksOf chunkSize (Foldl.generalize collectIndex) writeIndexChunks
  where
    moveResult :: (DiskIdx.OnDiskIndex term doc p, ReleaseKey)
               -> m (DiskIdx.OnDiskIndex term doc p)
    moveResult (DiskIdx.OnDiskIndex path, key) = do
        liftIO $ renameDirectory path outputPath
        release key  -- will fail
        return (DiskIdx.OnDiskIndex outputPath)
{-# INLINEABLE buildIndex #-}

treeReduce :: forall m a. (m ~ SafeT IO)
           => Int   -- ^ maximum reduction width
           -> ([a] -> m a)
           -> [a]
           -> m a
treeReduce width f xs0 = do
    n <- liftIO getNumCapabilities
    withLimit <- concurrencyLimited n
    xs0Async <- mapM (async . return) xs0
    go withLimit xs0Async
  where
    go :: (m a -> m a) -> [Async a] -> m a
    go _withLimit [xAsync] = wait xAsync
    go withLimit xsAsync  = do
        let run :: [Async a] -> m (Async a)
            run ysAsync = async $ do
                ys' <- mapM wait ysAsync
                withLimit $ f ys'

        let reductions = chunksOf width xsAsync
        reducedAsyncs <- mapM run reductions
        go withLimit reducedAsyncs
{-# INLINEABLE treeReduce #-}

-- | Perform the final merge of a set of index chunks.
mergeIndexChunks :: (MonadSafe m, Binary term, Ord term, Binary doc, Binary p)
            => [(DiskIdx.OnDiskIndex term doc p, ReleaseKey)]
            -> m (DiskIdx.OnDiskIndex term doc p, ReleaseKey)
mergeIndexChunks chunks = do
    let (chunkFiles, chunkKeys) = unzip chunks
    idxs <- liftIO $ mapM DiskIdx.openOnDiskIndex chunkFiles
    (path, key) <- newTempDir
    liftIO $ DiskIdx.merge path idxs
    mapM_ release chunkKeys
    return (DiskIdx.OnDiskIndex path, key)
{-# INLINEABLE mergeIndexChunks #-}

newTempDir :: (MonadSafe m)
           => m (FilePath, ReleaseKey)
newTempDir = do
    liftIO $ createDirectoryIfMissing True ".build-index"
    path <- liftIO $ createTempDirectory ".build-index" "part.index"
    key <- register $ liftIO $ removeDirectoryRecursive path
    return (path, key)

-- | Write a set of index chunks.
writeIndexChunks :: forall term doc p m. (MonadSafe m, Binary term, Ord term, Binary doc, Binary p)
             => Foldl.FoldM m ([(DocumentId, doc)], M.Map term [Posting p])
                              [(DiskIdx.OnDiskIndex term doc p, ReleaseKey)]
writeIndexChunks =
    premapM' chunkToIndex
    $ Foldl.generalize Foldl.list
  where
    chunkToIndex :: ([(DocumentId, doc)], M.Map term [Posting p])
                 -> m (DiskIdx.OnDiskIndex term doc p, ReleaseKey)
    chunkToIndex (docIdx, postingIdx) = do
        (path, key) <- newTempDir
        liftIO $ DiskIdx.fromDocuments path docIdx postingIdx
        return (DiskIdx.OnDiskIndex path, key)
{-# INLINEABLE writeIndexChunks #-}

-- | Build an index chunk in memory.
collectIndex :: forall term p doc. (Ord term)
             => Foldl.Fold (doc, M.Map term p)
                           ([(DocumentId, doc)], M.Map term [Posting p])
collectIndex =
    zipFold (DocId 0) succ
    ((,) <$> docIdx <*> termIdx)
  where
    docIdx :: Foldl.Fold (DocumentId, (doc, M.Map term p)) [(DocumentId, doc)]
    docIdx =
        lmap (\(docId, (meta, _)) -> (docId, meta)) Foldl.list

    termIdx :: Foldl.Fold (DocumentId, (doc, M.Map term p)) (M.Map term [Posting p])
    termIdx =
        lmap (\(docId, (_, terms)) -> foldMap (toPosting docId) $ M.toList terms) mconcatMaps

    toPosting :: DocumentId -> (term, p) -> M.Map term [Posting p]
    toPosting docId (term, p) = M.singleton term [Posting docId p]
{-# INLINEABLE collectIndex #-}
