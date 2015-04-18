{-# LANGUAGE RankNTypes #-}
module Data.Machine.IO.Concurrent
  ( toChan
  , fromChan
  , mergeIO
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

forkChan :: Foldable t => t (MachineT IO k a) -> IO (Chan a)
forkChan xs = do
  c <- newChan
  mapM_ (forkIO . runT_ . (~> toChan c)) xs
  return c

mergeIO
  :: (MonadIO m, Foldable t)
  => t (MachineT IO k a)  -- | A collection of machines
  -> IO (MachineT m k' a) -- | A MonadIO'ic machine
mergeIO xs = fmap fromChan $ forkChan xs
