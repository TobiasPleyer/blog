#!/usr/bin/env stack
{- stack script
   --resolver lts-11.8
   --package base
-}

import Data.Char (toUpper)

echo_shout :: IO ()
echo_shout = do
  line <- getLine
  putStrLn $ (map toUpper line) ++ "!"

main = echo_shout
