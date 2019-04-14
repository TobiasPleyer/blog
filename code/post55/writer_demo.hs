#!/usr/bin/env stack
{- stack script
   --resolver lts-11.8
   --package transformers
-}

import Control.Monad.Trans.Writer.Lazy

writer_computation :: Writer String Int
writer_computation = do
  writer (1,"Hello")
  writer (2," World")
  writer (3,"!!!")

main = do
  print $ runWriter writer_computation
