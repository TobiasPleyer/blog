module Main where

import Control.Monad
import Conduit
import Data.Conduit
import Data.Conduit.Internal.Conduit ((=$=))
import Data.Conduit.Internal.Pipe (Pipe(..), runPipe, injectLeftovers)


main :: IO ()
main = do
         print 2
         print 3
         runPipe $ go [] $ Done()
  where

    go ls (HaveOutput p o) = HaveOutput (go ls p) o
    go (l:ls) (NeedInput p _) = go ls $ p l
    go [] (NeedInput p c) = NeedInput (go [] . p) (go [] . c)
    go _ (Done r) = Done r
    go ls (PipeM mp) = PipeM (liftM (go ls) mp)
    go ls (Leftover p l) = go (l:ls) p
