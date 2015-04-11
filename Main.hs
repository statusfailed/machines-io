{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Monad.IO.Class
import Control.Concurrent hiding (yield)
import Control.Concurrent.Chan

import Data.Machine
import Data.Machine.IO.Concurrent


countDelay :: Int -> SourceT IO Int
countDelay us = construct (go 0)
  where
    go n = do
      yield n
      liftIO $ threadDelay us
      go (n + 1)

printer :: (Show a, MonadIO m) => ProcessT m a ()
printer = repeatedly (await >>= liftIO . print)

main = do
  m <- mergeIO [left, right]
  runT_ $ m ~> printer
  print 1
  where
    left :: SourceT IO (Either Int String)
    left  = fmap Left (countDelay 100000)

    right :: SourceT IO (Either Int String)
    right = fmap (Right . show) (countDelay 500000)
