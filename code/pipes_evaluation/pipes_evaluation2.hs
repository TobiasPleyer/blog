module Main where

import Pipes
import qualified Pipes.Prelude as PP


my_producer :: Proxy X () () Int IO ()
my_producer = do
  yield 1
  yield 2


my_consumer :: Proxy () Int () X IO ()
my_consumer = loop
  where
    loop = do
      i <- await
      lift $ print (i+1)
      loop

main :: IO ()
main = runEffect $ my_producer >-> my_consumer
