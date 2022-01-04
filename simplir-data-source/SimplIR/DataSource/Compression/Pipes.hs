module SimplIR.DataSource.Compression.Pipes
    ( -- * Compression schemes
      Compression(..)
      -- * Pipes-based interface
    , decompressed
    , decompressWith
    ) where

import           Control.Monad (join)

import           Pipes
import qualified Pipes.Internal as P
import           Pipes.Safe (SafeT, runSafeT, MonadSafe, MonadThrow)
import           Pipes.ByteString as P.BS
import qualified Pipes.BZip as P.BZip
import qualified Pipes.GZip as P.GZip
import qualified Pipes.Lzma as P.Lzma
import SimplIR.DataSource.Compression

-- | Decompress a stream, determining the compression scheme via the header.
decompressed :: (MonadThrow m, MonadSafe m)
             => Producer ByteString m () -> Producer ByteString m ()
decompressed prod0 = do
    res <- lift $ next prod0
    case res of
      Right (bs0, rest) -> do
          let compression = detectCompression bs0
          decompressWith compression (yield bs0 >> rest)
      Left r -> return r

-- | Decompress a stream with the given 'Compression' scheme.
decompressWith
    :: (MonadThrow m, MonadSafe m)
    => Maybe Compression
    -> Producer ByteString m () -> Producer ByteString m ()
decompressWith Nothing      = id
decompressWith (Just GZip)  = decompressGZip
decompressWith (Just BZip2) = decompressBZip2
decompressWith (Just Lzma)  = join . P.Lzma.decompress

decompressBZip2
    :: (MonadThrow m, MonadSafe m)
    => Producer ByteString m () -> Producer ByteString m ()
decompressBZip2 = P.BZip.decompress params
  where
    params = P.BZip.DecompressParams
        { P.BZip.dpVerbosity = 0
        , P.BZip.dpSmall = False
        }

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
