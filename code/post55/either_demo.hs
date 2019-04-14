#!/usr/bin/env stack
{- stack script --resolver lts-11.8 -}

ok_computation :: Either String Int
ok_computation = Right(1) >> Right(2) >> Right(3) >> Right(4)

nok_computation :: Either String Int
nok_computation = Right(1) >> Right(2) >> Left("Something bad happened") >> Right(4)

main = do
  print ok_computation
  print nok_computation
