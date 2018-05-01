{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Text.HTML.Scalpel
import Text.HTML.TagSoup


data ArticleInfo = ArticleInfo
  { articleHeadline :: String
  , articleAuthor :: String
  , articleUrl :: String
  } deriving (Show)

-- Global Variables

nytUrl = "https://www.nytimes.com"

articleSelector = ("div" @: [hasClass "collection",
                             notP (hasClass "headlines")])
                  //
                  ("article" @: [hasClass "story",
                                 hasClass "theme-summary",
                                 notP (hasClass "banner")])
headlineSelector = "h2" @: [hasClass "story-heading"]
authorSelector = "p" @: [hasClass "byline"]
urlSelector = "h2" // "a"

-- Utility functions

extract :: Scraper String ArticleInfo
extract = do
  headline <- text headlineSelector
  author <- text authorSelector
  url <- attr "href" urlSelector
  return (ArticleInfo headline author url)

articleScraper :: Scraper String [ArticleInfo]
articleScraper = chroots articleSelector extract

printResult :: Maybe ([ArticleInfo]) -> IO ()
printResult Nothing = putStrLn "Failed to parse any articles"
printResult (Just articles) = forM_ articles (\article ->
    do
      putStrLn ""
      putStrLn (articleHeadline article)
      putStrLn (articleAuthor article)
      putStrLn ("Url: " ++ (articleUrl article)))

main :: IO ()
main = do
  scrapeResult <- scrapeURL nytUrl articleScraper
  printResult scrapeResult
