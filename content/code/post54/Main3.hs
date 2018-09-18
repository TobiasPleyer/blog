module Main where

import Conduit


my_producer :: ConduitT i Int IO ()
my_producer = yield 1 >>= (\_ -> yield 2)


my_consumer :: ConduitT Int o IO ()
my_consumer = loop
  where
    loop = await >>= (\mi -> handle mi)
    handle mi =
      case mi of
        Just i -> do
          liftIO $ print(i+1)
          loop
        Nothing -> return ()

main :: IO ()
main = runConduit $ my_producer .| my_consumer
