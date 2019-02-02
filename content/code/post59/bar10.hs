bar' :: Cont r String
bar' = ContT $ \c1 -> runContT (
      ContT $ \_ -> (\x -> runContT (ret x) c1) "They say hello."
      ) (\x -> runContT (ret_f x) (\x -> runContT (ret x) c1))
  where
    ret_f = \_ -> return ("They appear to be saying " ++ (show "hello"))
    ret = \msg -> return msg
