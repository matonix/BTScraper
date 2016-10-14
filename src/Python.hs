module Python
  ( scrapePythonURL
  , makePythonStats
  , makePythonStatsCSV
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

makePythonStatsCSV :: FilePath -> IO [Stats]
makePythonStatsCSV csvFile = do
  Right csv <- readCSV csvFile
  catMaybes <$> mapM makePythonStats (getIssueIds csv)

getIssueIds :: Csv -> [Int]
getIssueIds = map fst . mapMaybe (BS.readInt . (V.! 1)) . V.toList

-- for scraping python issue

type PythonFieldSet = [ByteString]
type PythonMessage  = ByteString
type PythonHistory  = [ByteString]

data PythonIssue = PythonIssue
  { pythonFieldSets :: [PythonFieldSet]
  , pythonMessages  :: [PythonMessage]
  , pythonHistories :: [PythonHistory]
  } deriving Show

scrapePythonURL :: String -> IO (Maybe PythonIssue)
scrapePythonURL url = scrapeURL url issue

makePythonStats :: Int -> IO (Maybe Stats)
makePythonStats issueId =
  fmap (parsePythonIssue issueId) <$> scrapeURL url issue where
    url = "http://bugs.python.org/issue" ++ show issueId

issue :: Scraper ByteString PythonIssue
issue = PythonIssue <$> fieldSets <*> messages <*> histories

fieldSets :: Scraper ByteString [PythonFieldSet]
fieldSets = chroots ("fieldset" // "table") $ texts "td"

messages :: Scraper ByteString [PythonMessage]
messages = texts ("a" @: ["href" @=~ regexCommitId]) where
  regexCommitId = mkRegex "http://hg.python.org/lookup/" :: Regex

histories :: Scraper ByteString [PythonHistory]
histories = chroots ("table" @: [hasClass "table-striped"] // "tr") $ texts "td"

-- parse into stats

parsePythonIssue :: Int -> PythonIssue -> Stats
parsePythonIssue inum issue = Stats
  inum
  (parsePeriod $ pythonHistories issue)
  (parsePriority $ pythonFieldSets issue)
  (parseReopen $ pythonHistories issue)
  (parseCommits $ pythonMessages issue)

parsePeriod :: [PythonHistory] -> Int
parsePeriod hists = read . init . show $ diffUTCTime new old where
  time = parseTime . BS.unpack . (!! 0)
  new = time $ head hists
  old = time $ last hists
  parseTime :: String -> UTCTime
  parseTime = parseTimeOrError True defaultTimeLocale "%F\160\&%T"

parsePriority :: [PythonFieldSet] -> ByteString
parsePriority = (!! 6) . (!! 1)

parseReopen :: [PythonHistory] -> Int
parseReopen = pred . length . filter containsClosed . map (!! 3) where
  containsClosed = BS.isInfixOf $ BS.pack "-> closed"

parseCommits :: [PythonMessage] -> [ByteString]
parseCommits = id
