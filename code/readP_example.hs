#!/usr/bin/env stack
{- stack
   script
  --resolver lts-9.12
  --package base
-}

import System.Environment (getArgs)
import Text.ParserCombinators.ReadP

my_parser = do
  c1 <- get        -- get the first char, no conditions
  char 'x'         -- expect the next char to be 'x'
  c2 <- get        -- get one more char
  return [c1, c2]  -- return the first and the second chars

main = do
  s <- head <$> getArgs            -- retrieve the first command line argument
  let rs = readP_to_S my_parser s  -- apply the parser to the string s
  if null rs                       -- check the return value of the parser
    then putStrLn "Parser failed"  -- empty list means no results produced
    else print $ head rs           -- take the first result and print it
