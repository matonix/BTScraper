module Main where

import           Control.Exception.Base
import           System.Directory
import           System.Environment     (getArgs)
import           Python
import           Stats
import           Cache

-- url = "http://bugs.python.org/issue24022"
workingDir = "/home/maton/experimentBTS/"
queryCsv = "query02.csv"
statsCsv = "stats02.csv"

main :: IO ()
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
