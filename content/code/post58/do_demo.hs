#!/usr/bin/env stack
{- stack script
   --resolver lts-11.8
   --package base
-}

-- File do_demo.hs

main = do
  putStr "How many gos do you want? "
  goCnt <- do
    line <- getLine
    let cnt = read line :: Int
    return cnt
  sequence_ $ replicate goCnt (putStr "go")
  putStrLn "!"
