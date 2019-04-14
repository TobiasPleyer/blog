bar' :: Cont r String
bar' = ContT $ \c1 -> f k
  where
    f = (\_ -> (\x -> runContT (ret x) c1) "They say hello.")
    k = \x -> runContT (ret_f x) (\x -> runContT (ret x) c1)
    ret_f = \_ -> return ("They appear to be saying " ++ (show "hello"))
    ret = \msg -> return msg
