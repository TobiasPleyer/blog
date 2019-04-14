import Control.Monad.Cont
import Control.Monad.Cont.Class

bar :: String -> Cont r String
bar s = do
    msg <- callCC $ \k -> do
        when (s == "hello") $ k "They say hello."
        return ("They appear to be saying " ++ (show s))
    return msg

main :: IO ()
main = do
  let msg1 = runCont (bar "hello") id
  putStrLn ("msg1: " ++ msg1)
  let msg2 = runCont (bar "other") id
  putStrLn ("msg2: " ++ msg2)
