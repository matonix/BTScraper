module Main where

import           Cache
import           Control.Exception.Base
import           Data.Maybe
import           Github
import           GithubCommit
import           Jira
import           Python
import           Stats
import           System.Directory
import           System.Environment     (getArgs)

workingDir = "/home/maton/experimentBTS/"
queryCsv = "Lang-commit-issue-db.csv"
statsCsv = "Lang-stats.csv"
main = scrapeWithCache queryPath makeJiraStatsCSV $ writeStats statsPath where
  queryPath = workingDir ++ queryCsv
  statsPath = workingDir ++ statsCsv

-- db = "/home/maton/experimentBTS/Lang-commit-issue-db.csv"
-- main = makeJiraStatsCSV db >>= print

-- db = "/home/maton/experimentBTS/Lang-commit-issue-db.csv"
-- url = "https://issues.apache.org/jira/browse/LANG-747?page=com.googlecode.jira-suite-utilities:transitions-summary-tabpanel"
-- main = makeJiraStats url db >>= print

-- workingDir = "/home/maton/experimentBTS/"
-- inFile = workingDir ++ "Math-commit-db.csv"
-- outFile = workingDir ++ "Math-commit-issue-db.csv"
-- logFile = workingDir ++ "commons-math-log"
-- main = linkGitlogCommitToIssueCSV logFile inFile outFile

-- commit = "1dfad5043a207e032a78ef50c3cba50488bcd300"
-- prefix = "https://github.com/google/closure-compiler/commit/"
-- main = linkGithubCommitToIssue prefix commit -- test

-- inFile = workingDir ++ "Time-commit-db.csv"
-- outFile = workingDir ++ "Time-commit-issue-db.csv"
-- prefix = "https://github.com/JodaOrg/joda-time/commits/"
-- main = linkGithubCommitToIssueCSV prefix inFile outFile

-- url = "https://github.com/google/closure-compiler/issues/2049"
-- main = scrapeGithubURL url >>= print

-- issue = 2049
-- main = scrapeGithubIssue issue >>= printStats . maybeToList

-- workingDir = "/home/maton/experimentBTS/"
-- queryCsv = "query02.csv"
-- statsCsv = "stats02.csv"
-- main = scrapeWithCache queryPath makePythonStatsCSV $ writeStats statsPath where
--   queryPath = workingDir ++ queryCsv
--   statsPath = workingDir ++ statsCsv

-- main = do
--   args <- getArgs
--   if length args /= 2
--     then putStrLn "Usage: cmd queryCsv statsCsv"
--     else do
--       workingDir <-  getCurrentDirectory
--       let queryPath = workingDir ++ args !! 0
--       let statsPath = workingDir ++ args !! 1
--       scrapeWithCache queryPath makePythonStatsCSV $ writeStats statsPath
