#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.12
  --package base
  --package data-fix
  --package containers
-}
{-# Language OverloadedStrings, DeriveTraversable #-}

import           Data.Fix
import           Data.Map (Map)
import qualified Data.Map as Map


data Node a = Node !Int [a]
  deriving (Show, Functor, Foldable, Traversable)

data Summary = Summary
  { weight :: !Int        -- ^ total node weight
  , children :: [Summary] -- ^ the children summaries
  } deriving Show

input = Map.fromList ([ ("pbga", Node 66 [])
                      , ("xhth", Node 57 [])
                      , ("ebii", Node 61 [])
                      , ("havc", Node 66 [])
                      , ("ktlj", Node 57 [])
                      , ("fwft", Node 72 ["ktlj","cntj","xhth"])
                      , ("qoyq", Node 66 [])
                      , ("padx", Node 45 ["pbga","havc","qoyq"])
                      , ("tknk", Node 41 ["ugml","padx","fwft"])
                      , ("jptl", Node 61 [])
                      , ("ugml", Node 68 ["gyxo","ebii","jptl"])
                      , ("gyxo", Node 61 [])
                      , ("cntj", Node 57 [])])

root_node = "tknk"

psi :: String -> Node String
psi s = input Map.! s

phi :: Node Summary -> Summary
phi (Node n xs) =
  if (null xs)
  then
    Summary n []
  else
    Summary (n + (sum . (map weight)) xs) xs

summarize = hylo phi psi

main = do
  let summary = summarize root_node
  print summary
