module Jira
  ( scrapeJiraURL
  , parseIssue
  , makeJiraStats
  , makeJiraStatsCSV
  ) where

import           Control.Monad
import           Csv
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Char                  (isDigit)
import           Data.Csv
import           Data.Maybe                 (catMaybes, fromJust)
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import           Debug.Trace
import           GithubCommit               (CommitIssue)
import qualified GithubCommit               as G
import           Stats                      (Stats)
import qualified Stats                      as S
import           Text.HTML.Scalpel

data JiraIssue = JiraIssue
  { jiraIssueId    :: ByteString
  , jiraPriority   :: ByteString
  , jiraTransition :: JiraTransition
  } deriving Show

data JiraTransition = JiraTransition
  { transition         :: [ByteString]
  , timeInSourceStatus :: [ByteString]
  , executionTimes     :: [ByteString]
  , lastExecuter       :: [ByteString]
  , lastExecutionDate  :: [ByteString]
  } deriving Show

makeJiraStatsCSV :: String -> FilePath -> IO [Stats]
makeJiraStatsCSV prefixId dbCsv = do
  Right db <- fmap V.toList <$> readCSV dbCsv
  let prefix = "https://issues.apache.org/jira/browse/" ++ prefixId
  let suffix = "?page=com.googlecode.jira-suite-utilities:transitions-summary-tabpanel"
  let urls = map ((\i -> prefix++show i++suffix) . G.issueId) db :: [URL]
  map (parseIssue db) . catMaybes <$> mapM scrapeJiraURL urls

-- for test
makeJiraStats :: URL -> FilePath -> IO Stats
makeJiraStats url dbCsv = do
  Just issue <- scrapeJiraURL url
  Right db <- fmap V.toList <$> readCSV dbCsv
  return $ parseIssue db issue

scrapeJiraURL :: URL -> IO (Maybe JiraIssue)
scrapeJiraURL url = scrapeURL url scrapeIssue

scrapeIssue :: Scraper ByteString JiraIssue
scrapeIssue = JiraIssue
  <$> scrapeIssueId
  <*> scrapePriority
  <*> scrapeJiraTransitions

scrapeIssueId :: Scraper ByteString ByteString
scrapeIssueId = attr "content" $ "meta" @: ["name" @= "ajs-issue-key"]

scrapePriority :: Scraper ByteString ByteString
scrapePriority = attr "alt" $ "span" @: ["id" @= "priority-val"] // "img"

scrapeJiraTransitions :: Scraper ByteString JiraTransition
scrapeJiraTransitions = chroot ("div" @: ["id" @= "issue_actions_container"])
  $ JiraTransition
  <$> texts ("td" @: ["width" @= "40%"])
  <*> texts ("td" @: ["width" @= "15%", "align" @= "left"])
  <*> texts ("td" @: ["width" @= "15%", "align" @= "center"])
  <*> texts ("td" @: ["width" @= "18%"] // "a")
  <*> texts ("td" @: ["width" @= "18%", "align" @= "right"])

parseIssue :: [CommitIssue] -> JiraIssue -> Stats
parseIssue db ji = S.Stats issueid period priority reopen commits
  where
    issueid = fst . fromJust . BS.readInt . BS.filter isDigit . jiraIssueId $ ji
    period = sum . map parseTimeToSeconds . timeInSourceStatus . jiraTransition $ ji
    priority = jiraPriority ji
    reopen = (`div` 2) . length . filter isReopen . transition . jiraTransition $ ji
    commits = map G.newCommitId $ filter ((== issueid) . fst . fromJust . BS.readInt . G.issueId) db

parseTimeToSeconds :: ByteString -> Int
parseTimeToSeconds = sum . map parseEach . filter ((>1) . BS.length) . BS.words
  where
    parseEach str = case BS.last str of
      'd' -> (*86400) .  fst . fromJust $ BS.readInt str
      'h' -> (*3600) . fst . fromJust $ BS.readInt str
      'm' -> (*60) . fst . fromJust $ BS.readInt str
      's' -> fst . fromJust $ BS.readInt str
      _ -> error $ "parseTimeToSeconds: no parse \"" ++ show str ++ "\""

isReopen :: ByteString -> Bool
isReopen = BS.isInfixOf (BS.pack "Reopened")
