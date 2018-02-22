module Main where

import Pipes
import Pipes.Internal
import qualified Pipes.Prelude as PP


my_producer :: Proxy X () () Int IO ()
my_producer = Respond 1 (\b' -> go (Pure b'))
  where
    go p = case p of
        Request a' fa  -> Request a' (\a  -> go (fa  a ))
        Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
        M          m   -> M (m >>= \p' -> return (go p'))
        Pure    r      -> Respond 2 (\r -> Pure r)


my_consumer :: Proxy () Int () X IO ()
my_consumer = loop
  where
    loop = Request () (\a -> go (Pure a))
      where
        go p = case p of
            Request a' fa  -> Request a' (\a  -> go (fa  a ))
            Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
            M          m   -> M (m >>= \p' -> return (go p'))
            Pure    r      -> (\i -> ((lift (print (i+1))) `_bind` (\_ -> loop))) r

main :: IO ()
main = runEffect $ my_producer >-> my_consumer
