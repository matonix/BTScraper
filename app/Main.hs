module Main where

import           Cache
import           Control.Exception.Base
import           Python
import           Stats
import           System.Directory
import           System.Environment     (getArgs)

-- url = "http://bugs.python.org/issue24022"
workingDir = "/home/maton/experimentBTS/"
queryCsv = "query03.csv"
statsCsv = "stats03.csv"

main :: IO ()
-- main = scrapePythonURL url >>= print
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
