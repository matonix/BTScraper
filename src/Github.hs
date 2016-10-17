module Github
  ( scrapeGithubURL
  , makeGithubStats
  , makeGithubStatsCSV
  ) where

import           Control.Monad
import           Csv
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Csv                   (Csv)
import           Data.Fixed
import           Data.Int
import           Data.Maybe                 (catMaybes, fromJust, mapMaybe)
import           Data.Time
import qualified Data.Vector                as V
import           Debug.Trace
import           Foreign.C.Types
import           GithubCommit               (CommitIssue)
import qualified GithubCommit               as G
import           Stats                      (Stats)
import qualified Stats                      as S
import           Text.HTML.Scalpel
import           Text.Regex

-- scrapeGithubCSV :: FilePath -> IO [Stats]
-- scrapeGithubCSV csvFile = do
--   Right csv <- readCSV csvFile
--   catMaybes <$> mapM scrapeGithubIssue (getIssueIds csv)

-- getIssueIds :: Csv -> [Int]
-- getIssueIds = map fst . mapMaybe (BS.readInt . (V.! 1)) . V.toList

-- for scraping github issue

data GithubIssue = GithubIssue
  { issueId     :: ByteString
  , openedTime  :: ByteString
  , closedTime  :: [ByteString]
  , stateClosed :: [ByteString]
  } deriving Show

makeGithubStatsCSV :: String -> FilePath -> IO [Stats]
makeGithubStatsCSV prefix dbCsv = do
  Right db <- fmap V.toList <$> readCSV dbCsv
  let urls = map ((prefix++) . BS.unpack . G.issueId) db :: [URL]
  return urls
  map (parseIssue db) . catMaybes <$> mapM scrapeGithubURL urls

-- for test
makeGithubStats :: URL -> FilePath -> IO Stats
makeGithubStats url dbCsv = do
  Just issue <- scrapeGithubURL url
  Right db <- fmap V.toList <$> readCSV dbCsv
  return $ parseIssue db issue

scrapeGithubURL :: URL -> IO (Maybe GithubIssue)
scrapeGithubURL url = scrapeURL url scrapeIssue

scrapeIssue :: Scraper ByteString GithubIssue
scrapeIssue = GithubIssue
  <$> text ("span" @: [hasClass "gh-header-number"])
  <*> chroot ("div" @: [hasClass "flex-table-item-primary"]) (attr "datetime" "relative-time")
  <*> chroots ("div" @: [hasClass "discussion-item-closed"]) (attr "datetime" "relative-time")
  <*> texts ("div" @: [hasClass "state-closed"])

-- parse into stats

parseIssue :: [CommitIssue] -> GithubIssue -> Stats
parseIssue db gi = S.Stats iId period priority reopen commits
  where
    iId = fst . fromJust . BS.readInt . BS.tail $ issueId gi
    period = parsePeriod (openedTime gi) (closedTime gi)
    priority = BS.pack "not available"
    reopen = pred . length $ stateClosed gi
    commits = map G.newCommitId $ filter ((== iId) . fst . fromJust . BS.readInt . G.issueId) db

parsePeriod :: ByteString -> [ByteString] -> Int
parsePeriod opened closed = read . init . show $ diffUTCTime new old where
  new = parseTime . BS.unpack $ last closed
  old = parseTime $ BS.unpack opened
  -- "2014-04-29T20:28:46Z"
  parseTime :: String -> UTCTime
  parseTime = parseTimeOrError True defaultTimeLocale "%FT%TZ"
