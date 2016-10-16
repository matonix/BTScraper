module Github
  ( scrapeGithubURL
  , makeGithubStats
  -- , makeGithubStatsCSV
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
import           Stats
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
  { issueId       :: ByteString
  , relativeTimes :: [ByteString]
  , stateClosed   :: [ByteString]
  , commitId      :: [ByteString]
  } deriving Show

scrapeGithubURL :: URL -> IO (Maybe GithubIssue)
scrapeGithubURL url = scrapeURL url scrapeIssue

makeGithubStats :: Int -> IO (Maybe Stats)
makeGithubStats iId = fmap parseIssue <$> scrapeGithubURL url
  where
    url = "https://github.com/google/closure-compiler/issues/" ++ show iId

scrapeIssue :: Scraper ByteString GithubIssue
scrapeIssue = GithubIssue
  <$> text ("span" @: [hasClass "gh-header-number"])
  <*> texts "relative-time"
  <*> texts ("svg" @: [hasClass "octicon-issue-closed"])
  <*> chroots ("td" @: [hasClass "commit-meta"]) (attr "href" "a")

-- parse into stats

parseIssue :: GithubIssue -> Stats
parseIssue issue = Stats
  (parseIssueId $ issueId issue)
  (parsePeriod $ relativeTimes issue)
  (BS.pack "not available")
  (parseReopen $ stateClosed issue)
  (parseCommits $ commitId issue)
  where
    parseIssueId = fst . fromJust . BS.readInt . BS.tail
    parsePeriod relativeTimes = read . init . show $ diffUTCTime new old where
      new = minimum times
      old = maximum times
      times = map (parseTime . BS.unpack) relativeTimes
      -- Sep 29, 2016
      parseTime :: String -> UTCTime
      parseTime = parseTimeOrError True defaultTimeLocale "%b %e, %Y"
    parseReopen = pred . length
    parseCommits = map (last . BS.split '/')
