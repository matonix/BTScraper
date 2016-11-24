module Main where

import           Cache
import           Control.Exception.Base
import           Data.Maybe
import           Github
-- import           GithubCommit
-- import           Jira
import           JiraJson
import           Python
import           Stats
import           System.Directory
import           System.Environment     (getArgs)

-- workingDir = "/home/maton/experimentBTS/issues2014-2016/"
-- queryCsv = "joda-time.csv"
-- statsCsv = "time-stats.csv"
-- prefix = "https://github.com/JodaOrg/joda-time/issues/"
-- main = scrapeWithCache queryPath (makeGithubStatsCSV2 prefix) $ writeStats statsPath where
--   queryPath = workingDir ++ queryCsv
--   statsPath = workingDir ++ statsCsv

-- workingDir = "/home/maton/experimentBTS/"
-- queryCsv = "Time-commit-issue-db2.csv"
-- statsCsv = "Time-stats2.csv"
-- prefix = "https://github.com/JodaOrg/joda-time/issues/"
-- main = scrapeWithCache queryPath (makeGithubStatsCSV prefix) $ writeStats statsPath where
--   queryPath = workingDir ++ queryCsv
--   statsPath = workingDir ++ statsCsv

-- workingDir = "/home/maton/experimentBTS/"
-- queryCsv = "Closure-commit-issue-db.csv"
-- statsCsv = "Closure-stats2.csv"
-- prefix = "https://github.com/google/closure-compiler/issues/"
-- main = scrapeWithCache queryPath (makeGithubStatsCSV prefix) $ writeStats statsPath where
--   queryPath = workingDir ++ queryCsv
--   statsPath = workingDir ++ statsCsv

-- db = "/home/maton/experimentBTS/Closure-commit-issue-db.csv"
-- main = makeGithubStatsCSV db >>= print

-- url = "https://github.com/JodaOrg/joda-time/issues/93"
-- db = "/home/maton/experimentBTS/Time-commit-issue-db2.csv"
-- main = makeGithubStats url db >>= print

-- url = "https://github.com/JodaOrg/joda-time/issues/93"
-- main = scrapeGithubURL url >>= print

main = scrapeJira

-- workingDir = "/home/maton/experimentBTS/issues2014-2016/"
-- prefixId = "MATH-"
-- queryCsv = "commons-lang.csv"
-- statsCsv = "lang-stats.csv"
-- prefixId = "MATH-"
-- queryCsv = "commons-math.csv"
-- statsCsv = "math-stats.csv"
-- main = scrapeWithCache queryPath (makeJiraStatsCSV2 prefixId) $ writeStats statsPath where
--   queryPath = workingDir ++ queryCsv
--   statsPath = workingDir ++ statsCsv

-- db = "/home/maton/experimentBTS/issues2014-2016/commons-math.csv"
-- main = makeJiraStatsCSV2 "MATH-" db >>= print

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

-- url = "https://github.com/google/closure-compiler/issues/936"
-- main = scrapeGithubURL url >>= print

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
