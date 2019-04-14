module Main where

import Pipes
import Pipes.Internal
import qualified Pipes.Prelude as PP


my_producer :: Proxy X () () Int IO ()
my_producer = (yield 1) `_bind` (\_ -> yield 2)


my_consumer :: Proxy () Int () X IO ()
my_consumer = loop
  where
    loop = await `_bind` (\i -> ((lift (print (i+1))) `_bind` (\_ -> loop)))

main :: IO ()
main = runEffect $ my_producer >-> my_consumer
