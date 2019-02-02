bar :: String -> Cont r String
bar s = check >>= ret
  where
    check = callCC (\k -> do
      when (s == "hello") $ k "They say hello."
      return ("They appear to be saying " ++ (show s)))
    ret = \msg -> return msg
