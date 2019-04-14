main :: IO ()
main = do
  let msg1 = runCont (bar') id
           = runCont (ContT $ \c1 -> f k) id
           = (\c1 -> f k) id
           = ((\x -> runContT (ret x) id) "They say hello.")
           = runContT (ret "They say hello.") id
           = runContT (return "They say hello.") id
           = runContT (ContT ($ "They say hello.")) id
           = ($ "They say hello.") id
           = id "They say hello."
           = "They say hello."
  putStrLn ("msg1: " ++ msg1)
