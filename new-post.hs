#!/usr/bin/env stack
{- stack
   script
   --resolver=lts-13.8
   --package base
   --package text
   --package time
-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Time.Clock
import           Data.Time.Calendar
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.Environment (getArgs)


type Y = Integer
type M = Int
type D = Int


getDate :: IO (Y,M,D)
getDate = (toGregorian . utctDay) <$> getCurrentTime


mkDateString :: Y -> M -> D -> String
mkDateString y m d = year ++ "-" ++ month ++ "-" ++ day
  where
    year = show y
    month = if m > 9 then show m else "0" ++ show m
    day = if d > 9 then show d else "0" ++ show d


main = do
  title <- (T.pack . head) <$> getArgs
  tmpl <- TIO.readFile "templates/new-post.markdown"
  (y,m,d) <- getDate
  let date = T.pack $ mkDateString y m d
      content = T.replace "DATE" date $
                T.replace "TITLE" title tmpl
      filepath = T.unpack $ "posts/" <> date <> "-" <> (T.replace " " "-" title) <> ".markdown"
  TIO.writeFile filepath content
