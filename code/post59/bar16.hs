bar' :: Cont r String
bar' = ContT $ \c1 -> runContT (ret_f ()) (\x -> runContT (ret x) c1)
  where
    ret_f = \_ -> return ("They appear to be saying " ++ (show "other"))
    ret = \msg -> return msg

main :: IO ()
main = do
  let msg2 = runCont bar2' id
           = runCont (ContT $ \c1 -> runContT (ret_f ()) (\x -> runContT (ret x) c1)) id
           = runContT (ret_f ()) (\x -> runContT (ret x) id)
           = runContT (return ("They appear to be saying " ++ (show "other"))) (\x -> runContT (ret x) id)
           = runContT (ContT ($ ("They appear to be saying " ++ (show "other")))) (\x -> runContT (ret x) id)
           = ($ ("They appear to be saying " ++ (show "other"))) (\x -> runContT (ret x) id)
           = (\x -> runContT (ret x) id) ("They appear to be saying " ++ (show "other"))
           = runContT (return "They appear to be saying \"other\"") id
           = "They appear to be saying \"other\""
  putStrLn ("msg2: " ++ msg2)
