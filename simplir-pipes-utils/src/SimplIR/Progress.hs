module SimplIR.Progress
    ( ProgressVar
    , newProgressVar
    , addProgress
    , finished
    , pollProgress
    , progressMessage
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
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

addProgress :: (MonadIO m, Monoid a) => ProgressVar a -> a -> m ()
addProgress var = updateProgress var . Progress

newProgressVar :: (Monoid a) => IO (ProgressVar a)
newProgressVar = ProgressVar <$> newIORef mempty

finished :: (MonadIO m, Monoid a) => ProgressVar a -> m ()
finished v = updateProgress v Finished

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

progressMessage
    :: (Real a, Show a)
    => Int      -- ^ width
    -> Maybe a  -- ^ maximum, if known
    -> a        -- ^ current
    -> String
progressMessage _barWidth Nothing cur = show cur
progressMessage barWidth (Just maximum) cur =
    let fract :: Double
        fract = realToFrac cur / realToFrac maximum
        bar = renderProgressBar (barWidth - 5) maximum cur
    in printf "%2.1f%%    %s" (100*fract) bar

renderProgressBar
    :: (Real a)
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
