bar :: String -> Cont r String
bar s = ContT $ \c1 -> runContT (
      when (s == "hello") (ContT $ \_ -> (\x -> runContT (ret x) c1) "They say hello.")
      ) (\x -> runContT (ret_f x) (\x -> runContT (ret x) c1))
  where
    ret_f = \_ -> return ("They appear to be saying " ++ (show s))
    ret = \msg -> return msg
