module Main where

import Conduit
import Data.Conduit
import qualified Data.Conduit.Internal.Conduit as C
import qualified Data.Conduit.Internal.Pipe as P


my_producer :: ConduitT i Int IO ()
my_producer = ConduitT (\rest  -> P.HaveOutput (rest  ()) 1) >>= (\_ ->
              ConduitT (\rest2 -> P.HaveOutput (rest2 ()) 2))


my_consumer :: ConduitT Int o IO ()
my_consumer = loop
  where
    loop = ConduitT (\f -> P.NeedInput (f . Just) (const $ f Nothing)) >>= (\mi -> handle mi)
    handle mi =
      case mi of
        Just i -> do
          liftIO $ print(i+1)
          loop
        Nothing -> return ()

main :: IO ()
main = runConduit $ my_producer .| my_consumer
