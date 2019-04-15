#!/usr/bin/env stack
{- stack
   script
   --resolver lts-13.8
   --package attoparsec
   --package base
   --package directory
   --package filepath
   --package pandoc
   --package text
-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad (forM_)
import qualified Data.Attoparsec.Text as P
import           Data.Char (isAlpha, isSpace)
import           Data.Either (fromRight, isLeft, isRight)
import           Data.List (intersperse)
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.Directory (listDirectory)
import           System.Environment (getArgs)
import           System.FilePath ((<.>), (</>))
import           Text.Pandoc


metadataParser :: P.Parser ( T.Text
                           -- ^ the key
                           , T.Text
                           -- ^ the value
                           )
metadataParser = do
  P.char ':'
  key <- P.takeWhile isAlpha
  P.char ':'
  value <- P.takeText
  return (key,T.strip value)

notEmpty :: T.Text -> Bool
notEmpty = T.any (not . isSpace)

getFirstNonEmpty :: [T.Text] -> Maybe T.Text
getFirstNonEmpty [] = Nothing
getFirstNonEmpty (t:ts)
  | notEmpty t = Just t
  | otherwise  = getFirstNonEmpty ts

sanitizeTitle :: T.Text -> String
sanitizeTitle = T.unpack
              . T.replace " " "-"
              . T.replace "'" ""
              . T.replace "\\" ""
              . T.replace "\"" ""
              . T.replace "." "-"
              . T.replace ":" "-"
              . T.strip

quote :: T.Text -> T.Text
quote t = "\"" <> t <> "\""

joinToLastEntry :: [T.Text] -> [T.Text] -> [T.Text]
joinToLastEntry [] ts' = [T.concat (intersperse " " (map T.strip ts'))]
joinToLastEntry ts ts' = init ts ++ [T.concat (intersperse " " (last ts : map T.strip ts'))]

replaceAssoc :: Eq a => a -> (b -> b) -> [(a,b)] -> [(a,b)]
replaceAssoc k f = go k f []
  where
    go k f seen [] = reverse seen
    go k f seen ((k',v):kvs)
      | k == k'   = go k f ((k',f v):seen) kvs
      | otherwise = go k f ((k',v):seen) kvs

main = do
  -- print $ P.parseOnly metadataParser ":tags: haskell, clojure"
  postDir <- head <$> getArgs
  posts <- listDirectory postDir
  forM_ posts $ \post -> do
    let postPath = postDir </> post
    putStrLn $ "Converting " ++ postPath
    content <- TIO.readFile postPath
    let
      ls = T.lines content
      (before,rest) = span (isLeft . P.parseOnly metadataParser) ls
      (metadataCandidate,afterCandidate) = span (isRight . P.parseOnly metadataParser) rest
      (nonEmptyFollowUpLines, after) = span notEmpty afterCandidate
      metadata = joinToLastEntry metadataCandidate nonEmptyFollowUpLines
      title = quote $ fromJust $ getFirstNonEmpty before
      kvs :: [(T.Text,T.Text)]
      kvs = map (fromRight ("","") . P.parseOnly metadataParser) metadata
      kvsWithTitle = ("title"," " <> title) : kvs
      kvsWithTitle' = replaceAssoc "summary" (quote . T.replace "\"" "\\\"") kvsWithTitle
      date = fromJust $ lookup "date" kvsWithTitle'
      mdPath = postDir </> sanitizeTitle (date <> "-" <> title) <.> "markdown"
      yamlMeta = T.unlines $ ["---"] ++ map (\(k,v) -> k <> ": " <> v) kvsWithTitle' ++ ["---",""]
    mdContent <- runIOorExplode $
                   readRST def{readerExtensions = pandocExtensions} (T.unlines (before ++ after))
                   >>= writeMarkdown def{writerExtensions = pandocExtensions}
    TIO.writeFile mdPath $ yamlMeta <> mdContent
