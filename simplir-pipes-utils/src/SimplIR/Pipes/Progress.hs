module SimplIR.Pipes.Progress
    ( ProgressVar
    , newProgressVar
    , progressPipe
    , pollProgress
    , finished
    , withFileProgress
    ) where

import           Control.Applicative
import qualified Data.ByteString as BS
import           Pipes
import qualified Pipes.Prelude as PP
import           Pipes.Safe
import qualified Pipes.ByteString as P.BS
import           System.IO
import           Data.Monoid
import           SimplIR.Progress

progressPipe :: (Monoid a, MonadIO m)
             => ProgressVar a
             -> (b -> a)
             -> Pipe b b m r
progressPipe var f = PP.mapM step
  where
    step x = do
        addProgress var (f x)
        return x

withFileProgress :: (MonadIO m, MonadSafe n)
                 => Handle
                 -> (Producer BS.ByteString m () -> n r)
                 -> n r
withFileProgress hdl k = do
    var <- liftIO newProgressVar
    sz <- liftIO $ hGetSize hdl
    let render (Sum n) = progressMessage 80 sz n
    liftIO $ pollProgress var 30 render
    register $ finished var
    r <- k (P.BS.fromHandle hdl >-> progressPipe var (Sum . fromIntegral . BS.length))
    return r

hGetSize :: Handle -> IO (Maybe Integer)
hGetSize hdl = fmap Just (hFileSize hdl) <|> return Nothing
