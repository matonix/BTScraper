{-# LANGUAGE DeriveGeneric #-}

module Jira
    ( scrapeJiraURL
    , parseIssue
    , makeJiraStats
    ) where

import           Control.Monad
import           Csv
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Char                  (isDigit)
import           Data.Csv
import           Data.Maybe                 (fromJust)
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import           Debug.Trace
import           GHC.Generics               (Generic)
import           Stats
import           Text.HTML.Scalpel

data JiraIssue = JiraIssue
  { jiraIssueNum   :: ByteString
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

data CommitIssue = CommitIssue
  { bugId       :: Int
  , oldCommitId :: ByteString
  , newCommitId :: ByteString
  , issueId     :: Int
  } deriving (Show,Generic)

instance FromRecord CommitIssue

makeJiraStats :: URL -> FilePath -> IO Stats
makeJiraStats url dbCsv = do
  Just issue <- scrapeJiraURL url
  Right db <- readCSV dbCsv
  return $ parseIssue issue (V.toList db)

scrapeJiraURL :: URL -> IO (Maybe JiraIssue)
scrapeJiraURL url = scrapeURL url scrapeIssue

scrapeIssue :: Scraper ByteString JiraIssue
scrapeIssue = JiraIssue
  <$> scrapeIssueNum
  <*> scrapePriority
  <*> scrapeJiraTransitions

scrapeIssueNum :: Scraper ByteString ByteString
scrapeIssueNum = attr "content" $ "meta" @: ["name" @= "ajs-issue-key"]

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

parseIssue :: JiraIssue -> [CommitIssue] -> Stats
parseIssue ji db = Stats issueNum period priority reopen commits
  where
    issueNum = fst . fromJust . BS.readInt . BS.filter isDigit . jiraIssueNum $ ji
    period = sum . map parseTimeToSeconds . timeInSourceStatus . jiraTransition $ ji
    priority = jiraPriority ji
    reopen = (`div` 2) . length . filter isReopen . transition . jiraTransition $ ji
    commits = map newCommitId $ filter ((== issueNum) . issueId) db

parseTimeToSeconds :: ByteString -> Int
parseTimeToSeconds = sum . map parseEach . filter ((>1) . BS.length) . BS.words
  where
    parseEach str = case BS.last str of
      'd' -> (*86400) .  fst . fromJust $ BS.readInt str
      'h' -> (*3600) . fst . fromJust $ BS.readInt str
      'm' -> (*60) . fst . fromJust $ BS.readInt str

isReopen :: ByteString -> Bool
isReopen = BS.isInfixOf (BS.pack "Reopened")
