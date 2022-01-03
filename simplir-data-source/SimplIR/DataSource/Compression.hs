{-# LANGUAGE OverloadedStrings #-}

module SimplIR.DataSource.Compression
    ( -- * Compression schemes
      Compression(..)
      -- * Lazy 'BSL.ByteString' interface
    , readCompressedFile
    , hGetCompressed
      -- * Pipes-based interface
    , decompressed
    , decompress
      -- * Utilities
    , guessCompressionFromFileName
    , detectCompression
    ) where

import           Control.Monad (join)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import           Control.Monad.Catch
import           System.IO
import           System.IO.Unsafe (unsafeInterleaveIO)
import           Control.Monad.Trans.Control ( MonadBaseControl(liftBaseWith) )

import           Pipes
import qualified Pipes.Internal as P
import           Pipes.Safe (SafeT, runSafeT, MonadSafe)
import           Pipes.ByteString as P.BS
import qualified Pipes.BZip as P.BZip
import qualified Pipes.GZip as P.GZip
import qualified Pipes.Lzma as P.Lzma

-- | A compression method
data Compression = GZip   -- ^ e.g. @file.gz@
                 | BZip2  -- ^ e.g. @file.bz@
                 | Lzma   -- ^ e.g. @file.xz@
                 deriving (Show, Ord, Eq)


-- | default method for reading files with as-needed decompression. 
readCompressedFile :: FilePath -> IO BSL.ByteString
readCompressedFile path =
    withFile path ReadMode hGetCompressed


-- | default method for reading streams with as-needed decompression (from a handle instead of a filename)
hGetCompressed :: Handle -> IO BSL.ByteString
hGetCompressed hdl =
    runSafeT $ fmap BSL.fromChunks $ go $ decompress Nothing (P.BS.fromHandle hdl)
  where
    go :: Producer BS.ByteString (SafeT IO) () -> SafeT IO [BS.ByteString]
    go (P.Pure ()) = return []
    go (P.M k) = k >>= go
    go (P.Request _ _) = error "hGetCompressed: impossible"
    go (P.Respond bs k) = do
        rest <- liftBaseWith $ \run -> unsafeInterleaveIO $ run $ go $ k ()
        return $ bs : rest


decompress :: (MonadThrow m, MonadSafe m)
           => Maybe Compression
           -> Producer ByteString m () -> Producer ByteString m ()
decompress Nothing      = id
decompress (Just GZip)  = decompressGZip
decompress (Just BZip2) = P.BZip.bunzip2
decompress (Just Lzma)  = join . P.Lzma.decompress

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

-- | Determine how a bit of data is compressed given some of its header.
detectCompression :: ByteString -> Maybe Compression
detectCompression s
  | "7zXZ" `BS.isPrefixOf` s     = Just Lzma
  | "\x1f\x8b" `BS.isPrefixOf` s = Just GZip
  | "BZh" `BS.isPrefixOf` s      = Just BZip2
  | otherwise                    = Nothing

decompressed :: (MonadThrow m, MonadSafe m)
             => Producer ByteString m () -> Producer ByteString m ()
decompressed prod0 = do
    res <- lift $ next prod0
    case res of
      Right (bs0, rest) -> do
          let compression = detectCompression bs0
          decompress compression (yield bs0 >> rest)
      Left r -> return r

-- | Try to identify the compression method of a file. 
-- | It is recomended to use `detectcCompression` instead.
guessCompressionFromFileName :: T.Text            -- ^ file name
                 -> Maybe Compression
guessCompressionFromFileName name
  | ".gz" `T.isSuffixOf` name  = Just GZip
  | ".xz" `T.isSuffixOf` name  = Just Lzma
  | ".bz2" `T.isSuffixOf` name = Just BZip2
  | otherwise                  = Nothing
