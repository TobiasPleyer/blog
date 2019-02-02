module Main where

import Control.Monad.Cont
import Control.Monad.Cont.Class

bar :: String -> Cont r String
bar s = ContT $ \c1 -> runContT (
      when (s == "hello") (ContT $ \_ -> (\x -> runContT (ret x) c1) "They say hello.")
      ) (\x -> runContT (ret_f x) (\x -> runContT (ret x) c1))
  where
    ret_f = \_ -> return ("They appear to be saying " ++ (show s))
    ret = \msg -> return msg

bar' :: Cont r String
bar' = ContT $ \c1 -> (\_ -> (\x -> runContT (ret x) c1) "They say hello.") (\x -> runContT (ret_f x) (\x -> runContT (ret x) c1))
  where
    ret_f = \_ -> return ("They appear to be saying " ++ (show "hello"))
    ret = \msg -> return msg

bar2' :: Cont r String
bar2' = ContT $ \c1 -> runContT (ret_f ()) (\x -> runContT (ret x) c1)
  where
    ret_f = \_ -> return ("They appear to be saying " ++ (show "other"))
    ret = \msg -> return msg

main :: IO ()
main = do
  let msg1 = runCont bar' id
  putStrLn ("msg1: " ++ msg1)
  let msg2 = runCont bar2' id
  putStrLn ("msg2: " ++ msg2)
