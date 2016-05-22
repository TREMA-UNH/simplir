module DataSource
   (
     -- * Data sources
     DataLocation(..)
   , produce
     -- * Compression
   , Compression(..)
   , decompress
   , withCompressedSource
   ) where

import           Data.ByteString (ByteString)
import           System.IO

import           Pipes
import           Pipes.Safe
import qualified Pipes.GZip as P.GZip
import qualified Pipes.Aws.S3 as P.S3
import qualified Pipes.ByteString as P.BS

data DataLocation = LocalFile { filePath :: FilePath }
                  | S3Object { s3Bucket :: P.S3.Bucket
                             , s3Object :: P.S3.Object
                             }

data Compression = GZip

decompress :: MonadIO m
           => Maybe Compression
           -> Producer ByteString m a -> Producer ByteString m a
decompress Nothing     = id
decompress (Just GZip) = decompressGZip

decompressGZip :: MonadIO m
               => Producer ByteString m r
               -> Producer ByteString m r
decompressGZip = go
  where
    go prod = do
        res <- P.GZip.decompress' prod
        case res of
            Left prod' -> go prod'
            Right r    -> return r

withCompressedSource :: MonadSafe m
                     => DataLocation -> Maybe Compression
                     -> (Producer ByteString m () -> m a)
                     -> m a
withCompressedSource loc compr action =
    action $ decompress compr (produce loc)

produce :: (MonadSafe m)
        => DataLocation
        -> Producer ByteString m ()
produce (LocalFile path) =
    bracket (liftIO $ openFile path ReadMode) (liftIO . hClose) P.BS.fromHandle
produce (S3Object bucket object) =
    P.S3.fromS3 bucket object $ \resp -> P.S3.responseBody resp
