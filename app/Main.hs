module Main where

import           Cache
import           Control.Exception.Base
import           Data.Maybe
import           Github
import           Python
import           Stats
import           System.Directory
import           System.Environment     (getArgs)

issue = 2049
url = "https://github.com/composer/composer/issues/4967"
-- url = "https://github.com/google/closure-compiler/issues/2049"
-- url = "http://bugs.python.org/issue24022"
-- issue = 24022
-- workingDir = "/home/maton/experimentBTS/"
-- queryCsv = "query02.csv"
-- statsCsv = "stats02.csv"

main :: IO ()
-- main = scrapeGithubURL url >>= print
main = scrapeGithubIssue issue >>= printStats . maybeToList
-- main = scrapePythonCSV queryPath >>= writeStats statsPath where
-- main = scrapeWithCache queryPath scrapePythonCSV $ writeStats statsPath where
--   queryPath = workingDir ++ queryCsv
--   statsPath = workingDir ++ statsCsv
-- main = do
--   args <- getArgs
--   if length args /= 2
--     then putStrLn "Usage: cmd queryCsv statsCsv"
--     else do
--       workingDir <-  getCurrentDirectory
--       let queryPath = workingDir ++ "/" ++ args !! 0
--       let statsPath = workingDir ++ "/" ++ args !! 1
--       scrapeWithCache queryPath scrapePythonCSV $ writeStats statsPath
