bar :: String -> Cont r String
bar s = ContT $ \c1 -> runContT (
    ContT $ \c -> runContT (
      when_f (\x -> ContT $ \_ -> (\x -> runContT (ret x) c1) x)
      ) (\x -> runContT (ret_f x) c)
    ) (\x -> runContT (ret x) c1)
  where
    when_f k = when (s == "hello") $ k "They say hello."
    ret_f = \_ -> return ("They appear to be saying " ++ (show s))
    ret = \msg -> return msg
