module Github
  ( scrapeGithubURL
  -- , scrapeGithubIssue
  -- , scrapeGithubCSV
  ) where

-- scrapeGithubCSV :: FilePath -> IO [Stats]
-- scrapeGithubCSV csvFile = do
--   Right csv <- parseCSV csvFile
--   catMaybes <$> mapM scrapeGithubIssue (getIssueNums csv)
--
-- parseCSV :: FilePath -> IO (Either String Csv)
-- parseCSV csvFile = decode NoHeader <$> BL.readFile csvFile
--
-- getIssueNums :: Csv -> [Int]
-- getIssueNums = map fst . mapMaybe (BS.readInt . (V.! 1)) . V.toList

-- for scraping github issue

type RelativeTime = ByteString
type StateClosed = ByteString
type CommitId = ByteString

data GithubIssue = GithubIssue
  { relativeTimes :: [RelativeTime]
  , stateClosed :: [StateClosed]
  , commitId :: [CommitId]
  } deriving Show

scrapeGithubURL :: String -> IO (Maybe GithubIssue)
scrapeGithubURL url = scrapeURL url issue

issue :: Spraper ByteString GithubIssue
issue = GithubIssue <$> relativeTime <*> stateClosed <*> commitId

relativeTimes :: Scraper ByteString [RelativeTime]
relativeTimes = texts "relative-time"

stateClosed :: Scraper ByteString [StateClosed]
stateClosed = texts ("div" @: [hasClass "state-closed"])

commitId :: Scraper ByteString [CommitId]
commitId = chroots ("td" @: [hasClass "commit-meta"]) $ attr "a"

-- parse into stats

parseGithubIssue :: Int -> GithubIssue -> Stats
parseGithubIssue inum issue = Stats
  { issueNum  = inum
  , period    = parsePeriod $ relativeTimes issue
  , priority  = "not available"
  , reopen    = parseReopen $ stateClosed issue
  , commits   = parseCommits $ commitId issue
  }

parsePeriod :: [RelativeTime] -> Int
parsePeriod relativeTimes = maximum times - minimum times where
  times = map parseTime relativeTimes
  parseTime :: String -> UTCTime
  parseTime = parseTimeOrError True defaultTimeLocale "%F\160\&%T"

parseReopen :: [StateClosed] -> Int

parseCommits :: [parseCommits] -> [ByteString]
