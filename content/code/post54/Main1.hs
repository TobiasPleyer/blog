module Main where

import Conduit


my_producer :: ConduitT i Int IO ()
my_producer = do
  yield 1
  yield 2


my_consumer :: ConduitT Int o IO ()
my_consumer = loop
  where
    loop = do
      mi <- await
      case mi of
        Just i -> do
          liftIO $ print(i+1)
          loop
        Nothing -> return ()

main :: IO ()
main = runConduit $ my_producer .| my_consumer
