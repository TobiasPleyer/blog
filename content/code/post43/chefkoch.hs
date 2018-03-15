#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.12
  --package base
  --package HTTP
  --package process
  --package tagsoup
-}

import Data.Char
import Network.HTTP
import System.Process
import Text.HTML.TagSoup
import Text.StringLike


data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
             deriving (Eq, Show)


data Month = January
           | February
           | March
           | April
           | May
           | June
           | July
           | August
           | September
           | October
           | November
           | December
           deriving (Eq, Show)


type Year = Int


data Unit = Count
          | Liter
          | Gram
          | SmallSpoon
          | BigSpoon
          deriving (Eq, Show)


data Recipe = Recipe
    { day :: Int
    , weekday :: Weekday
    , month :: Month
    , year :: Year
    , name :: String
    , link :: String
    , ingredients :: [( String  -- ^ The name of the ingredient
                      , Float   -- ^ The quantity
                      , Unit    -- ^ The unit corresponding to the quantity
                      )]
    , instruction :: String
    } deriving (Eq, Show)


month2Int :: Month -> Int
month2Int January = 1
month2Int February = 2
month2Int March = 3
month2Int April = 4
month2Int May = 5
month2Int June = 6
month2Int July = 7
month2Int August = 8
month2Int September = 9
month2Int October = 10
month2Int November = 11
month2Int December = 12


str2Weekday :: String -> Weekday
str2Weekday "(Mo)" = Monday
str2Weekday "(Di)" = Tuesday
str2Weekday "(Mi)" = Wednesday
str2Weekday "(Do)" = Thursday
str2Weekday "(Fr)" = Friday
str2Weekday "(Sa)" = Saturday
str2Weekday "(So)" = Sunday


openURL :: String -> IO String
openURL url = getResponseBody =<< simpleHTTP (getRequest url)


wgetURL :: String -> IO String
wgetURL url = do
    let
      wget_path = "/usr/bin/wget"
      wget_args = [url, "-qO-"]
    readProcess wget_path wget_args ""


getRecipeOverview :: (String -> IO String) -> Year -> Month -> IO String
getRecipeOverview grabber year month = do
    let url = ("https://www.chefkoch.de/rezept-des-tages.php?month="
               ++ show (month2Int month)
               ++ "&year="
               ++ show year)
    grabber url


normalize :: [Tag String] -> [Tag String]
normalize (x:xs) =
  case x of
    TagText t -> TagText (unwords (words t)) : normalize xs
    _         -> x : normalize xs
normalize [] = []


extractRecipeTable :: [Tag String] -> [Tag String]
extractRecipeTable = takeWhile (~/= TagClose "table")
                   . tail
                   . dropWhile (~/= TagOpen "table" [("class", "table-day")])


extractRecipesFromTable :: Year -> Month -> [Tag String] -> [Recipe]
extractRecipesFromTable year month = map (toRecipe . map clearTag)
                                   . splitList 4
                                   . filter (\t -> notEmptyText t && notTableTag t)
                                   . normalize
    where
      notEmptyText (TagText "") = False
      notEmptyText _            = True

      notTableTag (TagOpen  "tr" _)   = False
      notTableTag (TagClose "tr")     = False
      notTableTag (TagOpen  "td" _)   = False
      notTableTag (TagClose "td")     = False
      notTableTag (TagOpen  "span" _) = False
      notTableTag (TagClose "span")   = False
      notTableTag (TagClose "a")      = False
      notTableTag _                   = True

      splitList n xs
        | length xs < n = []
        | otherwise     = (take n xs) : splitList n (drop n xs)

      clearTag :: Tag String -> String
      clearTag (TagText t) = t
      clearTag tag@(TagOpen "a" attrs) = fromAttrib "href" tag
      clearTag _ = ""

      toRecipe [day, weekday, link, name] =
        Recipe
          (read (init day))
          (str2Weekday weekday)
          month
          year
          name
          ("https://www.chefkoch.de" ++ link)
          []
          ""


main = do
    let
      year = 2018
      month = February
    website <- getRecipeOverview wgetURL year month
    print $ extractRecipesFromTable year month $ extractRecipeTable $ parseTags $ website
