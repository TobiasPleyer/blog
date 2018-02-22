module Main where

import Pipes
import Pipes.Internal
import Pipes.Core
import qualified Pipes.Prelude as PP


main :: IO ()
main = do
  print 2
  driver ((Respond 2 (\r -> Pure r)) >>~ (\a -> go' (Pure a)))
    where
      go p = case p of
          Request a' fa  -> Request a' (\a  -> go (fa  a ))
          Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
          M          m   -> M (m >>= \p' -> return (go p'))
          Pure    r      -> Respond 2 (\r -> Pure r)
      go' p = case p of
          Request a' fa  -> Request a' (\a  -> go' (fa  a ))
          Respond b  fb' -> Respond b  (\b' -> go' (fb' b'))
          M          m   -> M (m >>= \p' -> return (go' p'))
          Pure    r      -> (\i -> ((lift (print (i+1))) `_bind` (\_ -> loop))) r
      go'' p = case p of
          Request a' fa  -> Request a' (\a  -> go'' (fa  a ))
          Respond b  fb' -> Respond b  (\b' -> go'' (fb' b'))
          M          m   -> M (m >>= \p' -> return (go'' p'))
          Pure    r      -> (\_ -> Request () (\a -> go' (Pure a))) r
      driver p = case p of
          Request v _ -> closed v
          Respond v _ -> closed v
          M       m   -> m >>= driver
          Pure    r   -> return r
      loop = Request () (\a -> go' (Pure a))
