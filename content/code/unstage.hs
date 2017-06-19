module Main where

import Control.Monad
import System.Directory
import System.Environment
import System.FilePath
import Data.Time.Clock
import Data.List

main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: unstage directory"
    (d:xs) -> do
      putStrLn d
      dirs <- getDirectoryContents d
      let abs_dirs = map (d </>) (filter noDot dirs)
      forM_ abs_dirs (sortAndDelete 3)


sortAndDelete limit branch = do
  putStrLn branch
  commit_infos <- getInfo branch
  let delete_candidates = drop limit (sortBy sortFunc commit_infos)
  forM_ delete_candidates (removeDirectoryRecursive . fst)
  where
    sortFunc a b = compare (snd b) (snd a)


getInfo branch = do
  commits <- getDirectoryContents branch
  let abs_commits = map (branch </>) (filter noDot commits)
  mod_times <- mapM getModificationTime abs_commits
  return (zip abs_commits mod_times)


noDot :: FilePath -> Bool
noDot "." = False
noDot ".." = False
noDot _ = True
