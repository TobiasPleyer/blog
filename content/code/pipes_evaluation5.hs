module Main where

import Pipes
import Pipes.Internal
import qualified Pipes.Prelude as PP


my_producer :: Proxy X () () Int IO ()
my_producer = (Respond 1 (\r -> Pure r)) `_bind` (\_ -> Respond 2 (\r -> Pure r))


my_consumer :: Proxy () Int () X IO ()
my_consumer = loop
  where
    loop = (Request () (\r -> Pure r)) `_bind` (\i -> ((lift (print (i+1))) `_bind` (\_ -> loop)))

main :: IO ()
main = runEffect $ my_producer >-> my_consumer
