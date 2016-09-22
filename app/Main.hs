module Main where

-- import System.Environment (getArgs)
import Python

url = "http://bugs.python.org/issue24022"

main :: IO ()
main = scrapePythonURL url >>= print
-- main = do
--   args <- getArgs
--   if length args /= 1
--     then usage = putStrLn "Usage: cmd filename"
--     else do
--       let filename = head args
