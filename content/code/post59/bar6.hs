bar :: String -> Cont r String
bar s = ContT $ \c1 -> runContT (f (\x -> ContT $ \_ -> (\x -> runContT (ret x) c1) x)) (\x -> runContT (ret x) c1)
  where
    f k = (when_f k) >>= ret_f
    when_f k = when (s == "hello") $ k "They say hello."
    ret_f = \_ -> return ("They appear to be saying " ++ (show s))
    ret = \msg -> return msg
