module Main where

import System.Environment (getArgs)
import Python

-- url = "http://bugs.python.org/issue24022"
csv = "/home/maton/experimentBTS/query02.csv"

usage = putStrLn "Usage: cmd filename"

main :: IO ()
main = scrapePythonCsv csv >>= print
-- main = do
--   args <- getArgs
--   if length args /= 1
--     then usage
--     else do
--       let url = head args
--       scrapePythonURL url >>= print
