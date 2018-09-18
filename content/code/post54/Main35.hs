module Main where

import Control.Monad
import Conduit
import Data.Conduit
import Data.Conduit.Internal.Conduit ((=$=))
import Data.Conduit.Internal.Pipe (Pipe(..), runPipe, injectLeftovers)


main :: IO ()
main = do
         print 2
         runPipe $ go [] $ goRight (Done ())
                                   (PipeM (liftM (\a -> unConduitT loop Done) (print (2+1))))
  where
    f = \rest  -> HaveOutput (rest  ()) 1
    g = (\_ -> ConduitT (\rest2 -> HaveOutput (rest2 ()) 2))
    h = \r -> f (\a -> unConduitT (g a) r)
    f2 = (\f -> NeedInput (f . Just) (const $ f Nothing))
    g2 = (\h2 -> f2 (\a2 -> unConduitT (handle a2) h2))
    loop = ConduitT (\f -> NeedInput (f . Just) (const $ f Nothing)) >>= handle

    handle mi =
      case mi of
        Just i -> do
          liftIO $ print(i+1)
          loop
        Nothing -> return ()

    goRight left right =
          case right of
              HaveOutput p o    -> HaveOutput (recurse p) o
              NeedInput rp rc   -> goLeft rp rc left
              Done r2           -> Done r2
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

    go ls (HaveOutput p o) = HaveOutput (go ls p) o
    go (l:ls) (NeedInput p _) = go ls $ p l
    go [] (NeedInput p c) = NeedInput (go [] . p) (go [] . c)
    go _ (Done r) = Done r
    go ls (PipeM mp) = PipeM (liftM (go ls) mp)
    go ls (Leftover p l) = go (l:ls) p
