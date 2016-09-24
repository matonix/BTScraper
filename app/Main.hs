module Main where

import           Control.Exception.Base
import           System.Environment     (getArgs)
-- import System.IO
import           Python
import           Stats
import           Cache

-- url = "http://bugs.python.org/issue24022"
workingDir = "/home/maton/experimentBTS/"
csv = "query03.csv"

-- usage = putStrLn "Usage: cmd filename"

main :: IO ()
main = scrapeWithCache (workingDir ++ csv) scrapePythonCSV writeStats

-- main = do
--   args <- getArgs
--   if length args /= 1
--     then usage
--     else do
--       let url = head args
--       scrapePythonURL url >>= print
