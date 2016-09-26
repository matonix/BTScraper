module Main where

import           Cache
import           Control.Exception.Base
import           Data.Maybe
import           Python
import           Stats
import           System.Directory
import           System.Environment     (getArgs)

url = "http://bugs.python.org/issue24022"
issue = 24022
workingDir = "/home/maton/experimentBTS/"
queryCsv = "query02.csv"
statsCsv = "stats02.csv"

main :: IO ()
-- main = scrapePythonIssue issue >>= printStats . maybeToList
-- main = scrapePythonCSV queryPath >>= writeStats statsPath where
main = scrapeWithCache queryPath scrapePythonCSV $ writeStats statsPath where
  queryPath = workingDir ++ queryCsv
  statsPath = workingDir ++ statsCsv

-- main = do
--   args <- getArgs
--   if length args /= 1
--     then putStrLn "Usage: cmd filename"
--     else do
--       let fileName = head args
--       workingDir <-  getCurrentDirectory
--       scrapeWithCache (workingDir ++ fileName) scrapePythonCSV writeStats
