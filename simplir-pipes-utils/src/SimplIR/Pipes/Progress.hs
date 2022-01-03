module SimplIR.Pipes.Progress
   ( ProgressVar
   , newProgressVar
   , progressPipe
   , pollProgress
   , finished
   , withFileProgress
   ) where

import Data.IORef
import Control.Applicative
import Control.Monad (forever, void)
import Control.Concurrent (forkIO, threadDelay)
import qualified Data.ByteString as BS
import Pipes
import qualified Pipes.Prelude as PP
import Pipes.Safe
import qualified Pipes.ByteString as P.BS
import System.Directory (getFileSize)
import System.IO
import Data.Monoid
import Text.Printf

data Progress a = Finished | Progress a

instance Semigroup a => Semigroup (Progress a) where
    Progress a <> Progress b = Progress (a <> b)
    _ <> _ = Finished

instance Monoid a => Monoid (Progress a) where
    mempty = Progress mempty

data ProgressVar a = ProgressVar (IORef (Progress a))

updateProgress :: (MonadIO m, Monoid a) => ProgressVar a -> Progress a -> m ()
updateProgress (ProgressVar var) x =
    liftIO $ atomicModifyIORef var $ \acc -> (acc `mappend` x, ())

newProgressVar :: (Monoid a) => IO (ProgressVar a)
newProgressVar = ProgressVar <$> newIORef mempty

pollProgress :: ProgressVar a -> Float -> (a -> String) -> IO ()
pollProgress (ProgressVar var) period toMesg = void $ forkIO $ go
  where
    go = do
        progress <- readIORef var
        case progress of
          Finished -> return ()
          Progress x -> do
            putStrLn $ toMesg x
            threadDelay $ round $ period * 1000 * 1000
            go

progressPipe :: (Monoid a, MonadIO m)
             => ProgressVar a
             -> (b -> a)
             -> Pipe b b m r
progressPipe var f = PP.mapM step
  where
    step x = do
        updateProgress var (Progress $ f x)
        return x

finished :: (MonadIO m, Monoid a) => ProgressVar a -> m ()
finished v = updateProgress v Finished

renderProgressBar
    :: Real a
    => Int -- ^ width
    -> a   -- ^ maximum
    -> a   -- ^ current
    -> String
renderProgressBar width maximum cur =
    replicate (fromIntegral n) '=' ++ replicate (width - fromIntegral n) ' '
  where
    frac :: Double
    frac = min 1 $ max 0 $ realToFrac cur / realToFrac maximum
    n = truncate (frac * realToFrac width)

withFileProgress :: (MonadIO m, MonadSafe n)
                 => Handle
                 -> (Producer BS.ByteString m () -> n r)
                 -> n r
withFileProgress hdl k = do
    var <- liftIO newProgressVar
    sz <- liftIO $ hGetSize hdl
    let render (Sum n) =
          case sz of
            Just s -> let fract = realToFrac n / realToFrac s :: Double
                          barWidth = 80
                          bar = renderProgressBar barWidth s n
                      in printf "%2.1f%%    %s" (100*fract) bar
            Nothing -> show n
    liftIO $ pollProgress var 30 render
    register $ finished var
    r <- k (P.BS.fromHandle hdl >-> progressPipe var (Sum . fromIntegral . BS.length))
    return r

hGetSize :: Handle -> IO (Maybe Integer)
hGetSize hdl = fmap Just (hFileSize hdl) <|> return Nothing
