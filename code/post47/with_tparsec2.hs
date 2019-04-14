#!/usr/bin/env stack
{- stack --resolver lts-9.12 runghc
  --package tagsoup
  --package parsec
  --package parsec-tagsoup
-}


import Control.Monad (forM_)
import Text.Parsec hiding (satisfy)
import Text.Parsec.String
import Text.HTML.TagSoup
import Text.ParserCombinators.Parsec.Tag


notTag :: TagRep rep => rep -> TagParser String () (Tag String)
notTag t = satisfy (~/= t) <?> ("not (" ++ show(toTagRep t :: Tag String) ++ ")")


strip = unwords . words


main :: IO ()
main = do
  exampleHtml <- readFile "example2.html"
  let tags = parseTags exampleHtml
      parseResults = parse getTableHeaders "demo" tags
  printParseResults parseResults


printParseResults :: Either ParseError [String] -> IO ()
printParseResults (Left err) = print err
printParseResults (Right results) = forM_ results print


getTableHeaders = do
  skipMany (notTag (TagOpen "div" [("class","statistics")]))
  many getTableHeader


getTableHeader = do
  header <- tagP (TagOpen "div" [("class","statistics")])
                 (\_ -> do
                        tagOpen "h2"
                        header <- tagText
                        tagClose "h2"
                        return header)
  skipMany (notTag (TagOpen "div" [("class","statistics")]))
  return header
