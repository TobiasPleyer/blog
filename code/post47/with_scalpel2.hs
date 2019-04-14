#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.12
  --package scalpel
-}

{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel
import Control.Monad
import Control.Applicative


main :: IO ()
main = do
  exampleHtml <- readFile "example2.html"
  let scrapeResults = scrapeStringLike exampleHtml altTextAndImages
  printScrapeResults scrapeResults

printScrapeResults Nothing = putStrLn "Something went wrong!"
printScrapeResults (Just []) = putStrLn "Couldn't scrape anything!"
printScrapeResults (Just results) = forM_ results print

altTextAndImages :: Scraper String [String]
altTextAndImages =
    chroots ("div" @: [hasClass "statistics"]) (text "h2")
