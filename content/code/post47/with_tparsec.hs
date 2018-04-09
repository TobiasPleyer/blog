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


notEmpty :: Tag String -> Bool
notEmpty tag
  | Just "" <- maybeTagText tag = False
  | otherwise = True


strip :: Tag String -> Tag String
strip = fmap (unwords . words)


main :: IO ()
main = do
  exampleHtml <- readFile "example.html"
  let tags = (filter notEmpty . map strip) (parseTags exampleHtml)
      parseResults = parse getTableRows "demo" tags
  printParseResults parseResults


printParseResults :: Either ParseError [(String,String,Maybe String)] -> IO ()
printParseResults (Left err) = print err
printParseResults (Right results) = forM_ results print


getTableRows = do
  skipMany (notTag (TagOpen "table" [("class","interesting things")]))
  tagP (TagOpen "table" []) (\_ ->
          many (tagP (TagOpen "tr" []) (\_ -> do
                                              tagOpen "td"
                                              (txt1,_) <- parseCol
                                              tagClose "td"
                                              tagOpen "td"
                                              (txt2,lnk) <- parseCol
                                              tagClose "td"
                                              return (txt1,txt2,lnk))))


parseCol = do
  optional (tagOpen "b")
  lnk <- optionMaybe(do
            lnkOpen <- tagOpen "a"
            return $ fromAttrib "href" lnkOpen)
  txt <- tagText
  optional (tagClose "a")
  optional (tagClose "b")
  return (txt,lnk)
