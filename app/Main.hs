module Main where

import Control.Applicative
import System.Environment (getArgs)
import Text.HTML.Scalpel hiding (URL)
import Text.Printf
import Python

type HistoryBegin = String
type HistoryEnd = String
type Priority = String
type Reopen = Int

data Item = Item HistoryBegin HistoryEnd Priority Reopen

url = "http://bugs.python.org/issue24022"

main :: IO ()
main = print =<< scrapePythonURL url
-- main = do
--   args <- getArgs
--   if length args /= 1
--     then usage = putStrLn "Usage: cmd filename"
--     else do
--       let filename = head args
