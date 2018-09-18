module Main where

import Control.Monad
import Conduit
import Data.Conduit
import Data.Conduit.Internal.Conduit ((=$=))
import Data.Conduit.Internal.Pipe (Pipe(..))


main :: IO ()
main = runConduit $ (=$=) (ConduitT h) (ConduitT g2)
  where
    f = \rest  -> HaveOutput (rest  ()) 1
    g = (\_ -> ConduitT (\rest2 -> HaveOutput (rest2 ()) 2))
    h = \r -> f (\a -> unConduitT (g a) r)
    f2 = (\f -> NeedInput (f . Just) (const $ f Nothing))
    g2 = (\h2 -> f2 (\a2 -> unConduitT (handle a2) h2))
    loop = (ConduitT (\f -> NeedInput (f . Just) (const $ f Nothing)) >>= (\mi -> handle mi))

    handle mi =
      case mi of
        Just i -> do
          liftIO $ print(i+1)
          loop
        Nothing -> return ()
