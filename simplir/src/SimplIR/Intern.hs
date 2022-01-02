{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}

module SimplIR.Intern
    ( InternM
    , runInternM'
    , runInternM
    , internAll
    , intern
    ) where

import Data.Tuple
import Data.Hashable
import Control.Lens
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import GHC.Magic (oneShot)
import Control.Monad (ap)
import qualified Data.HashMap.Strict as HM

-- | The interning table
newtype Table b = Table (HM.HashMap b b)

emptyTable :: Table b
emptyTable = Table HM.empty

insert :: (Eq b, Hashable b)
       => b -> Table b -> (Table b, b)
insert b (Table m)
  | Just b' <- HM.lookup b m = (Table m, b')
  | otherwise                = (Table $ HM.insert b b m, b)
{-# INLINE insert #-}

newtype InternM b m a = InternM { unInternM :: Table b -> m (Table b, a) }
                      --deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

instance Monad m => Functor (InternM b m) where
  fmap f g = g >>= return . f

instance Monad m => Applicative (InternM b m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (InternM b m) where
  return x = InternM $ oneShot $ \table -> return (table, x)
  InternM f >>= g = InternM $ oneShot $ \table -> do
    !(!table, x) <- f table
    unInternM (g x) table

instance MonadTrans (InternM b) where
  lift m = InternM $ oneShot $ \table -> (table,) <$> m

instance MonadIO m => MonadIO (InternM b m) where
  liftIO = lift . liftIO

runInternM' :: forall b m a. Monad m => InternM b m a -> m (Table b, a)
runInternM' (InternM m) = do
  m emptyTable

runInternM :: forall b m a. Monad m => InternM b m a -> m a
runInternM = fmap snd . runInternM'

internAll :: (Monad m, Eq b, Hashable b)
          => Optical (->) (->) (InternM b m) a a b b  -- ^ e.g. @Traversal a a b b@
          -> a -> InternM b m a
internAll t = traverseOf t intern
{-# INLINE internAll #-}

intern :: (Monad m, Eq a, Hashable a) => a -> InternM a m a
intern x = InternM $ oneShot $ \table ->
    let !(!table', !x') = insert x table
    in return (table', x')
{-# INLINE intern #-}
