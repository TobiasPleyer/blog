#!/usr/bin/env stack
{- stack script
   --resolver lts-11.8
   --package base
-}

-- File do_demo_desugared.hs

main =
  putStr "How many gos do you want? " >>
    (
    getLine >>= \line ->
    return (read line :: Int) >>= \cnt ->
    return cnt
    ) >>= \goCnt ->
  (sequence_ $ replicate goCnt (putStr "go")) >>
  putStrLn "!"
