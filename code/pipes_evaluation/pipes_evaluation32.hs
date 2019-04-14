module Main where

import Pipes
import Pipes.Internal
import Pipes.Core
import qualified Pipes.Prelude as PP


main :: IO ()
main = do
  print 2
  print 3
  return ()
