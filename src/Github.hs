module Github
  ( scrapeGithubURL
  , scrapeGithubIssue
  -- , scrapeGithubCSV
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
import           Stats
import           Text.HTML.Scalpel          hiding (URL)
import           Text.Regex

-- scrapeGithubCSV :: FilePath -> IO [Stats]
-- scrapeGithubCSV csvFile = do
--   Right csv <- readCSV csvFile
--   catMaybes <$> mapM scrapeGithubIssue (getIssueIds csv)

-- getIssueIds :: Csv -> [Int]
-- getIssueIds = map fst . mapMaybe (BS.readInt . (V.! 1)) . V.toList

-- for scraping github issue

type RelativeTime = ByteString
type StateClosed = ByteString
type CommitId = ByteString

data GithubIssue = GithubIssue
  { githubRelativeTimes :: [RelativeTime]
  , githubStateClosed   :: [StateClosed]
  , githubCommitId      :: [CommitId]
  } deriving Show

scrapeGithubURL :: String -> IO (Maybe GithubIssue)
scrapeGithubURL url = scrapeURL url issue

scrapeGithubIssue :: Int -> IO (Maybe Stats)
scrapeGithubIssue issueId =
  fmap (parseGithubIssue issueId) <$> scrapeURL url issue where
    url = "https://github.com/google/closure-compiler/issues/" ++ show issueId

issue :: Scraper ByteString GithubIssue
issue = GithubIssue <$> relativeTimes <*> stateClosed <*> commitId

relativeTimes :: Scraper ByteString [RelativeTime]
relativeTimes = texts "relative-time"

stateClosed :: Scraper ByteString [StateClosed]
stateClosed = texts ("svg" @: [hasClass "octicon-issue-closed"])

commitId :: Scraper ByteString [CommitId]
commitId = chroots ("td" @: [hasClass "commit-meta"]) $ attr "href" "a"

-- parse into stats

parseGithubIssue :: Int -> GithubIssue -> Stats
parseGithubIssue inum issue = Stats
  inum
  (parsePeriod $ githubRelativeTimes issue)
  (BS.pack "not available")
  (parseReopen $ githubStateClosed issue)
  (parseCommits $ githubCommitId issue)

parsePeriod :: [RelativeTime] -> Int
parsePeriod relativeTimes = read . init . show $ diffUTCTime new old where
  new = minimum times
  old = maximum times
  times = map (parseTime . BS.unpack) relativeTimes
  -- Sep 29, 2016
  parseTime :: String -> UTCTime
  parseTime = parseTimeOrError True defaultTimeLocale "%b %e, %Y"

parseReopen :: [StateClosed] -> Int
parseReopen = pred . length

parseCommits :: [CommitId] -> [ByteString]
parseCommits = map (last . BS.split '/')
