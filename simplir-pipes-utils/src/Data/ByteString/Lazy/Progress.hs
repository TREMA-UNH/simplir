module Data.ByteString.Lazy.Progress
    ( readFile
    , hGetContents
    ) where

import Control.Applicative
import System.IO (Handle, hFileSize, openFile, IOMode(..))
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import SimplIR.Progress
import Data.Monoid(Sum(..))
import Prelude hiding (readFile)


readFile :: FilePath -> IO BSL.ByteString
readFile fpath = do
    hdl <- openFile fpath ReadMode
    hGetContents hdl

-- | Gets contents from a stream and prints progress information every 30 seconds.
hGetContents :: Handle -> IO BSL.ByteString
hGetContents hdl = do
    bsl <- BSL.hGetContents hdl
    var <- newProgressVar
    sz <- hGetSize hdl
    let render (Sum n) = progressMessage 80 sz n
    pollProgress var 30 render
    return $ bslProgress var bsl

bslProgress :: ProgressVar (Sum Integer) -> BSL.ByteString -> BSL.ByteString
bslProgress var =
    BSL.fromChunks . go . BSL.toChunks
  where
    go [] = unsafePerformIO $ finished var >> return []
    go (chunk:rest) = unsafePerformIO $ do
      addProgress var $ Sum $ fromIntegral $ BS.length chunk
      return $ chunk : go rest

hGetSize :: Handle -> IO (Maybe Integer)
hGetSize hdl = fmap Just (hFileSize hdl) <|> return Nothing
