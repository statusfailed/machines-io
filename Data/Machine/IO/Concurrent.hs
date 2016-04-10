{-# LANGUAGE RankNTypes #-}
module Data.Machine.IO.Concurrent
  ( toChan
  , fromChan
  {-, mergeIO-}
  ) where

import Data.Machine
import Control.Monad.IO.Class

import Control.Concurrent         hiding (yield)
import Control.Concurrent.Chan

toChan :: MonadIO m => Chan a -> ProcessT m a b
toChan c = repeatedly (await >>= liftIO . writeChan c)

fromChan :: MonadIO m => Chan a -> SourceT m a
fromChan c = repeatedly $ liftIO (readChan c) >>= yield

runChan :: MonadIO m => (m () -> IO ()) -> Chan a -> SourceT m a -> IO ()
runChan f c s = f . runT_ $ s ~> toChan c

forkChan
  :: (MonadIO m, Foldable t)
  => (forall v . m v -> IO v)
  -> t (MachineT m k a)
  -> IO (Chan a)
forkChan f xs = do
  c <- newChan
  mapM_ (forkIO . f . runT_ . (~> toChan c)) xs
  return c

mergeIO
  :: (MonadIO m, Foldable t)
  => (forall v. m v -> IO v)
  -> t (MachineT m k a)  -- | A collection of machines
  -> IO (MachineT m k' a) -- | A MonadIO'ic machine
mergeIO f xs = fmap fromChan $ forkChan f xs
