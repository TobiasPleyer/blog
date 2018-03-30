#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.12
  --package attoparsec
  --package bytestring
  --package time
-}

{-# LANGUAGE OverloadedStrings #-}

import Data.Word
import Data.Time
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import Control.Monad
import qualified Data.ByteString as B


logFile :: FilePath
logFile = "yesterday.log"


data IP = IP Word8 Word8 Word8 Word8 deriving Show

data Product = Mouse | Keyboard | Monitor | Speakers deriving Show

data LogEntry = LogEntry
    { entryTime :: LocalTime
    , entryIP   :: IP
    , entryProduct   :: Product
    } deriving Show

type Log = [LogEntry]


parseIP :: Parser IP
parseIP = do
  d1 <- decimal
  char '.'
  d2 <- decimal
  char '.'
  d3 <- decimal
  char '.'
  d4 <- decimal
  return $ IP d1 d2 d3 d4


timeParser :: Parser LocalTime
timeParser = do
  y  <- count 4 digit
  char '-'
  mm <- count 2 digit
  char '-'
  d  <- count 2 digit
  char ' '
  h  <- count 2 digit
  char ':'
  m  <- count 2 digit
  char ':'
  s  <- count 2 digit
  return $
    LocalTime { localDay = fromGregorian (read y) (read mm) (read d)
              , localTimeOfDay = TimeOfDay (read h) (read m) (read s)
                }


productParser :: Parser Product
productParser =
     (string "mouse"    >> return Mouse)
 <|> (string "keyboard" >> return Keyboard)
 <|> (string "monitor"  >> return Monitor)
 <|> (string "speakers" >> return Speakers)


logEntryParser :: Parser LogEntry
logEntryParser = do
  t <- timeParser
  char ' '
  ip <- parseIP
  char ' '
  p <- productParser
  return $ LogEntry t ip p

logParser :: Parser Log
logParser = many $ logEntryParser <* endOfLine


main :: IO ()
main = do
  log_content <- B.readFile logFile
  let parse_result = parseOnly logParser log_content
  case parse_result of
    Left err -> print err
    Right logs -> forM_ logs print
