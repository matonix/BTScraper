module Python
  ( scrapePythonURL
  , scrapePythonIssue
  , scrapePythonCSV
  ) where

import           Control.Monad
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Csv
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

scrapePythonCSV :: FilePath -> IO [Stats]
scrapePythonCSV csvFile = do
  Right csv <- parseCSV csvFile
  catMaybes <$> mapM scrapePythonIssue (getIssueNums csv)

parseCSV :: FilePath -> IO (Either String Csv)
parseCSV csvFile = decode NoHeader <$> BL.readFile csvFile

getIssueNums :: Csv -> [Int]
getIssueNums = map fst . mapMaybe (BS.readInt . (V.! 1)) . V.toList

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

scrapePythonIssue :: Int -> IO (Maybe Stats)
scrapePythonIssue issueNum =
  fmap (parsePythonIssue issueNum) <$> scrapeURL url issue where
    url = "http://bugs.python.org/issue" ++ show issueNum

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
  { issueNum  = inum
  , period    = parsePeriod $ pythonHistories issue
  , priority  = parsePriority $ pythonFieldSets issue
  , reopen    = parseReopen $ pythonHistories issue
  , commits   = parseCommits $ pythonMessages issue
  }

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
