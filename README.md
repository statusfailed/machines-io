# Concurrency utilities for `machines`

NOTE: this is mostly an experiment. I advise against using it!

# What is it for?

Simple use case: if you have two sources of data (say sockets),
and you want to do a blocking read on both, and merge the events

Example code below. Note this will finish by crashing, because
there are no more processes writing to the `Chan`.

    {-# LANGUAGE RankNTypes #-}
    module Main where

    import Data.Machine
    import Data.Machine.IO.Concurrent
    import Control.Monad.IO.Class
    import Control.Concurrent (newChan, forkIO, threadDelay)

    -- | Yield elements from a list with a specified delay in microseconds between each.
    slowSource :: MonadIO m => Int -> [a] -> ProcessT m () a
    slowSource t xs = construct $ go xs
      where
        go [] = return ()
        go (x:xs) = do
          liftIO $ threadDelay t -- wait 1 second
          yield x
          go xs

    main = do
      c <- newChan
      t1 <- forkIO (runT_ $ slowSource 500000  [1..4] ~> toChan c)
      t2 <- forkIO (runT_ $ slowSource 1000000 [1..4] ~> toChan c)
      runT_ $ fromChan c ~> autoM print
      print "done"

Running it:

    *Main> main
    1
    2
    1
    3
    4
    2
    3
    4
    *** Exception: thread blocked indefinitely in an MVar operation
