module Main where

import           Cache
import           Control.Exception.Base
import           Data.Maybe
import           Github
import           GithubCommit
import           Python
import           Stats
import           System.Directory
import           System.Environment     (getArgs)

commit = "1dfad5043a207e032a78ef50c3cba50488bcd300"
prefix = "https://github.com/JodaOrg/joda-time/commits/"
-- prefix = "https://github.com/google/closure-compiler/commit/"
-- inFile = workingDir ++ "Closure-commit-db-test.csv"
-- outFile = workingDir ++ "Closure-commit-issue-db-test.csv"
-- inFile = workingDir ++ "Closure-commit-db.csv"
-- outFile = workingDir ++ "Closure-commit-issue-db.csv"
inFile = workingDir ++ "Time-commit-db.csv"
outFile = workingDir ++ "Time-commit-issue-db.csv"
-- issue = 2049
-- url = "https://github.com/composer/composer/issues/4967"
-- url = "https://github.com/google/closure-compiler/issues/2049"
-- url = "http://bugs.python.org/issue24022"
-- issue = 24022
workingDir = "/home/maton/experimentBTS/"
-- queryCsv = "query02.csv"
-- statsCsv = "stats02.csv"

main :: IO ()
-- main = linkGithubCommitToIssue prefix commit
main = linkGithubCommitToIssueCSV prefix inFile outFile
-- main = scrapeGithubURL url >>= print
-- main = scrapeGithubIssue issue >>= printStats . maybeToList
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
