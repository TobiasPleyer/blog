bar :: String -> Cont r String
bar s = ContT $ \c1 -> runContT check (\x -> runContT (ret x) c1)
  where
    check = callCC f
    f k = do
      when (s == "hello") $ k "They say hello."
      return ("They appear to be saying " ++ (show s))
    ret = \msg -> return msg
