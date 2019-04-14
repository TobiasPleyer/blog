module Main where

import Control.Monad
import Conduit
import Data.Conduit
import Data.Conduit.Internal.Conduit ((=$=))
import Data.Conduit.Internal.Pipe (Pipe(..), runPipe, injectLeftovers)


main :: IO ()
main = runPipe $ injectLeftovers $ (\rest ->
    let
      goRight left right =
            case right of
                HaveOutput p o    -> HaveOutput (recurse p) o
                NeedInput rp rc   -> goLeft rp rc left
                Done r2           -> rest r2
                PipeM mp          -> PipeM (liftM recurse mp)
                Leftover right' i -> goRight (HaveOutput left i) right'
          where
            recurse = goRight left

      goLeft rp rc left =
          case left of
              HaveOutput left' o        -> goRight left' (rp o)
              NeedInput left' lc        -> NeedInput (recurse . left') (recurse . lc)
              Done r1                   -> goRight (Done r1) (rc r1)
              PipeM mp                  -> PipeM (liftM recurse mp)
              Leftover left' i          -> Leftover (recurse left') i
        where
          recurse = goLeft rp rc
    in goRight (h Done) (g2 Done)) Done
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
