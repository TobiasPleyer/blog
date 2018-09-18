module Main where

import Conduit
import Data.Conduit
import Data.Conduit.Internal.Pipe (Pipe(..))
import qualified Data.Conduit.Internal.Conduit as C
import qualified Data.Conduit.Internal.Pipe as P


my_producer :: ConduitT i Int IO ()
my_producer = ConduitT (\h -> f (\a -> unConduitT (g a) h))
  where
    f = \rest  -> HaveOutput (rest  ()) 1
    g = (\_ -> ConduitT (\rest2 -> HaveOutput (rest2 ()) 2))


my_consumer :: ConduitT Int o IO ()
my_consumer = loop
  where
    loop = ConduitT (\f -> NeedInput (f . Just) (const $ f Nothing)) >>= (\mi -> handle mi)
    handle mi =
      case mi of
        Just i -> do
          liftIO $ print(i+1)
          loop
        Nothing -> return ()

main :: IO ()
main = runConduit $ my_producer .| my_consumer
