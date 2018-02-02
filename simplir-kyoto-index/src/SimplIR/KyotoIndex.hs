{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SimplIR.KyotoIndex
    ( DiskIndexPath(..)
    , DiskIndex
    , indexPath
      -- * Creation
    , create
      -- * Adding documents
    , addDocuments
      -- * Opening
    , withIndex
    , Mode(..)
      -- * Queries
    , lookupPostings
    , Posting(..)
    , lookupPostings'
    , lookupDocument
    ) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Bits
import Data.Foldable
import Data.Hashable
import Data.Proxy
import Data.Semigroup
import Data.Word
import qualified Data.Array as A
import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Control.Foldl as Foldl
import qualified Database.KyotoCabinet.DB.Tree as K
import qualified Database.KyotoCabinet.Operations as K
import qualified Codec.Serialise as S
import System.FilePath
import System.Directory
import System.IO.Unsafe

import qualified SimplIR.EncodedList.Cbor as ELC
import qualified SimplIR.Encoded.Cbor as EC

newtype DiskIndexPath term termInfo doc p = DiskIndexPath { getDiskIndexPath :: FilePath }

docIndexPath :: DiskIndexPath term termInfo doc p -> FilePath
docIndexPath (DiskIndexPath p) = p </> "documents"

postingIndexPath :: DiskIndexPath term termInfo doc p -> FilePath
postingIndexPath (DiskIndexPath p) = p </> "postings"

notAtomic :: Bool
notAtomic = False

data Mode = ReadMode | ReadWriteMode

newtype DocId = DocId Word32
              deriving (Show, Enum, Eq, Ord, B.Binary, S.Serialise)

data Posting p = Posting !DocId p

instance S.Serialise p => S.Serialise (Posting p) where
    encode (Posting a b) = S.encode a <> S.encode (EC.encode b)
    decode = do
        a <- S.decode
        b <- EC.decode <$> S.decode
        return (Posting a b)

data BucketedLock = BucketedLock { buckets :: !(A.Array Int (TMVar ()))
                                 , bucketMask :: !Int
                                 }

newBucketedLock :: Int -> IO BucketedLock
newBucketedLock n = do
    let nBuckets = 1 `shiftL` n
        bucketMask = nBuckets - 1
    buckets <- A.listArray (0, nBuckets-1) <$> replicateM nBuckets (newTMVarIO ())
    return BucketedLock {..}

withBucketLock :: (MonadIO m, MonadMask m, Hashable a)
               => BucketedLock -> a -> m r -> m r
withBucketLock (BucketedLock{..}) x = bracket acquire release . const
  where
    !lock = buckets A.! (bucketMask .&. hash x)
    acquire = liftIO $ atomically $ takeTMVar lock
    release _ = liftIO $ atomically $ putTMVar lock ()

data DiskIndex (mode :: Mode) term termInfo doc p
    = DiskIndex { docIndex :: !K.Tree
                , postingIndex :: !K.Tree
                , nextDocId :: !(TVar DocId)
                , termLock :: !BucketedLock
                , indexPath :: DiskIndexPath term termInfo doc p
                , writable :: Bool -- TODO break out a new type
                }

-- $index-structure
--
-- The index consists of two B-tree databases: a posting index and a document index.
-- The document index contains:
--
--   * A @"next-docid"@ key containing a 'Binary'-serialised 'DocId'. This is
--     the next available document ID in the index.
--
--   * A value for each document (serialised with 'Serialise') keyed on its
--     document ID (serialised with 'Binary').
--
-- The posting index contains a set of entries for each term. Each begins with a prefix
-- of the length of the term's 'Serialise'-encoded representation, followed by
-- the representation itself:
--
--  * A term summary key has no further key suffix and contains a
--    'Serialise'-encoded @termInfo@
--  * Multiple term postings keys are suffixed with the first document ID of the
--    chunk. Each contains a @CborList (Posting p)@.

class IsWritable (mode :: Mode) where
    isWritable :: Proxy mode -> Bool

instance IsWritable ReadWriteMode where isWritable _ = True
instance IsWritable ReadMode where isWritable _ = False

-- | Open an on-disk index.
--
-- The path should be the directory of a valid 'DiskIndex'
open :: forall term termInfo doc p mode. (IsWritable mode)
     => DiskIndexPath term termInfo doc p -> IO (DiskIndex mode term termInfo doc p)
open indexPath = do
    let writable = isWritable $ Proxy @mode
        access
          | writable  = K.Writer [] [K.TryLock]
          | otherwise = K.Reader [K.NoLock, K.NoRepair]
    docIndex <- K.openTree (docIndexPath indexPath) loggingOpts access
    postingIndex <- K.openTree (postingIndexPath indexPath) loggingOpts access
    Just nextDocId <- K.get docIndex "next-docid"
    let Right (_, _, nextDocId') = B.decodeOrFail $ BSL.fromStrict nextDocId
    nextDocId <- newTVarIO nextDocId'
    termLock <- newBucketedLock 6
    return $ DiskIndex {..}
  where
    termLockBuckets = 128

withIndex :: (MonadMask m, MonadIO m, IsWritable mode)
          => DiskIndexPath term termInfo doc p
          -> (DiskIndex mode term termInfo doc p -> m a)
          -> m a
withIndex indexPath = bracket (liftIO $ open indexPath) (liftIO . close)

loggingOpts :: K.LoggingOptions
loggingOpts = K.defaultLoggingOptions

treeOpts :: K.TreeOptions
treeOpts = K.defaultTreeOptions
    { K.buckets = Just (8 * 1024 * 1024 * 1024) -- TODO: should be 10% record count
    , K.pageCacheSize = Just (4 * 1024 * 1024 * 1024)
    }

create :: forall term termInfo doc p. ()
       => FilePath -> IO (DiskIndexPath term termInfo doc p)
create dest = do
    let indexPath = DiskIndexPath dest
    createDirectory dest
    docIndex <- K.makeTree (docIndexPath indexPath) loggingOpts treeOpts (K.Writer [K.Create] [K.TryLock])
    postingIndex <- K.makeTree (postingIndexPath indexPath) loggingOpts treeOpts (K.Writer [K.Create] [K.TryLock])
    nextDocId <- newTVarIO $ DocId 1
    termLock <- newBucketedLock 1
    let writable = True
    close $ DiskIndex {..}
    return indexPath
  where
    treeOpts = K.defaultTreeOptions

close :: DiskIndex mode term termInfo doc p -> IO ()
close DiskIndex{..} = do
    docId <- atomically $ readTVar nextDocId
    when writable $ K.set docIndex "next-docid" (BSL.toStrict $ B.encode docId)
    K.close docIndex
    K.close postingIndex

postingsKey :: S.Serialise term => term -> DocId -> BS.ByteString
postingsKey term docId = BSL.toStrict $ B.runPut $
    B.putWord16le (fromIntegral $ BSL.length t') <> B.putLazyByteString t' <> B.put docId
  where t' = S.serialise term

termKey :: S.Serialise term => term -> BS.ByteString
termKey term = BSL.toStrict $ B.runPut $
    B.putWord16le (fromIntegral $ BSL.length t') <> B.putLazyByteString t'
  where t' = S.serialise term

postingsKeyDocId :: BS.ByteString -> DocId
postingsKeyDocId = B.runGet parse . BSL.fromStrict
  where
    parse = do
        len <- B.getWord16le
        B.skip $ fromIntegral len
        B.get

documentKey :: DocId -> BS.ByteString
documentKey = BSL.toStrict . B.encode

withTermLock :: Hashable term
             => DiskIndex 'ReadWriteMode term termInfo doc p -> term -> IO a -> IO a
withTermLock DiskIndex{..} = withBucketLock termLock

addDocuments
    :: forall doc p term termInfo m.
       (S.Serialise doc, S.Serialise p,
        S.Serialise term, Ord term, Hashable term,
        S.Serialise termInfo, Semigroup termInfo, MonadIO m)
    => DiskIndex 'ReadWriteMode term termInfo doc p
    -> Foldl.FoldM m (V.Vector (doc, M.Map term (termInfo, p))) Int
       -- ^ returns number of added documents
addDocuments idx@DiskIndex{..} = Foldl.FoldM step initial finish
  where
    step :: Int -> V.Vector (doc, M.Map term (termInfo, p)) -> m Int
    step !count docs = liftIO $ do
        let count' = count + V.length docs
        docId0 <- atomically $ do
            DocId docId <- readTVar nextDocId
            writeTVar nextDocId $ DocId $ docId + fromIntegral (V.length docs)
            return (DocId docId)

        K.setBulk docIndex
            [ (BSL.toStrict $ B.encode docId, BSL.toStrict $ S.serialise doc)
            | (docId, (doc, _termPostings)) <- zip [docId0..] $ V.toList docs
            ]
            notAtomic

        let inverted :: M.Map term (termInfo, Seq.Seq (Posting p))
            inverted = M.fromListWith (<>)
                [ (term, (termInfo, Seq.singleton (Posting docId posting)))
                | (docId, (_doc, termPostings)) <- zip [docId0..] $ V.toList docs
                , (term, (termInfo, posting)) <- M.assocs termPostings
                ]

        K.setBulk postingIndex
            [ ( postingsKey term docId0
              , BSL.toStrict $ S.serialise $ ELC.fromList $ toList $ postings
              )
            | (term, (_, postings)) <- M.assocs inverted
            ]
            notAtomic

        forM_ (M.assocs inverted) $ \(term, (termInfo, _)) -> withTermLock idx term $ do
            let key = termKey term
            mbTermInfo0 <- K.get postingIndex key
            case mbTermInfo0 of
              Just termInfo0 -> let Right termInfo0' = S.deserialiseOrFail $ BSL.fromStrict termInfo0
                                in K.replace postingIndex key $ BSL.toStrict $ S.serialise $ termInfo0' <> termInfo
              Nothing        -> K.set postingIndex key $ BSL.toStrict $ S.serialise termInfo

        return $! count'

    initial = return 0
    finish count = return count

-- | Lookup postings for a term.
lookupPostings
    :: forall term termInfo p doc.
       (S.Serialise termInfo, S.Serialise term, S.Serialise p, S.Serialise doc)
    => DiskIndex 'ReadMode term termInfo doc p
    -> term
    -> Maybe (termInfo, [(doc, p)])
lookupPostings idx term =
    fmap (fmap $ map f) $ lookupPostings' idx term
  where
    f (Posting docId p) = (lookupDocument idx docId, p)

-- | Lookup postings for a term.
lookupPostings'
    :: forall term termInfo p doc.
       (S.Serialise termInfo, S.Serialise term, S.Serialise p)
    => DiskIndex 'ReadMode term termInfo doc p
    -> term
    -> Maybe (termInfo, [Posting p])
-- It's unsafe to allow querying on Writeable indexes since term update is not
-- atomic.
lookupPostings' idx@DiskIndex{..} term = unsafePerformIO $ do
    cur <- K.cursor postingIndex
    K.curJumpKey cur termPrefix
    key <- K.curGetKey cur False
    if termPrefix `BS.isPrefixOf` key
      then do termInfo <- S.deserialise . BSL.fromStrict <$> K.curGetValue cur True
              postings <- unsafeInterleaveIO $ go cur
              return $ Just (termInfo, postings)
      else return Nothing
  where
    !termPrefix = termKey term

    go :: K.Cursor -> IO [Posting p]
    go cur = do
        (k,v) <- K.curGet cur True
        if termPrefix `BS.isPrefixOf` k
          then do let p = ELC.toList $ S.deserialise $ BSL.fromStrict v
                  rest <- unsafeInterleaveIO $ go cur
                  return (p ++ rest)
          else return []

lookupDocument :: S.Serialise doc
               => DiskIndex mode term termInfo doc p -> DocId -> doc
lookupDocument idx = unsafePerformIO . lookupDocumentIO idx

lookupDocumentIO :: S.Serialise doc
                 => DiskIndex mode term termInfo doc p -> DocId -> IO doc
lookupDocumentIO DiskIndex{..} doc = do
    Just bs <- K.get docIndex (BSL.toStrict $ B.encode doc)
    return $ S.deserialise $ BSL.fromStrict bs
