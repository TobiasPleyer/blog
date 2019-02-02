bar :: String -> Cont r String
bar s = ContT $ \c1 -> runContT check (\x -> runContT (ret x) c1)
  where
    check = ContT $ \c2 -> runContT (f (\x -> ContT $ \ _ -> c2 x)) c2
    f k = (when_f k) >>= ret_f
    when_f k = when (s == "hello") $ k "They say hello."
    ret_f = \_ -> return ("They appear to be saying " ++ (show s))
    ret = \msg -> return msg
