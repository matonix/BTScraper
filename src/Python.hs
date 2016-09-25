module Python
  ( scrapePythonURL
  , scrapePythonCSV
  ) where

import           Control.Monad
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Csv
import           Data.Fixed
import           Data.Maybe                 (catMaybes, fromJust, mapMaybe)
import           Data.Time
import qualified Data.Vector                as V
import           Debug.Trace
import           Foreign.C.Types
import           Stats
import           Text.HTML.Scalpel          hiding (URL)

scrapePythonCSV :: FilePath -> IO [Stats]
scrapePythonCSV csvFile = do
  Right csv <- parseCSV csvFile
  catMaybes <$> mapM scrapePythonURL (getIssueNums csv)

parseCSV :: FilePath -> IO (Either String Csv)
parseCSV csvFile = decode NoHeader <$> BL.readFile csvFile

getIssueNums :: Csv -> [Int]
getIssueNums = map fst . mapMaybe (BS.readInt . (V.! 1)) . V.toList

-- for scraping python issue

type PythonFieldSet = [ByteString]
type PythonHistory = [ByteString]
data PythonIssue = PythonIssue
  { pythonFieldSets :: [PythonFieldSet]
  , pythonHistories :: [PythonHistory]
  } deriving Show

scrapePythonURL :: Int -> IO (Maybe Stats)
scrapePythonURL issueNum =
  fmap (parsePythonIssue issueNum) <$> scrapeURL url issue where
    url = "http://bugs.python.org/issue" ++ show issueNum

issue :: Scraper ByteString PythonIssue
issue = PythonIssue <$> fieldSets <*> histories

fieldSets :: Scraper ByteString [PythonFieldSet]
fieldSets = chroots ("fieldset" // "table") $ texts "td"

histories :: Scraper ByteString [PythonHistory]
histories = chroots ("table" @: [hasClass "table-striped"] // "tr") $ texts "td"

parsePythonIssue :: Int -> PythonIssue -> Stats
parsePythonIssue inum issue = Stats
  { issueNum = inum
  , period = new `periodLocalTime` old
  , priority = pythonFieldSets issue !! 1 !! 6
  , reopen = pred . length . filter containsClosed $ map (!! 3) hists
  } where
    hists = pythonHistories issue :: [PythonHistory]
    time = parsePythonTime . BS.unpack . (!! 0)
    new = time $ head hists
    old = time $ last hists
    containsClosed = BS.isInfixOf $ BS.pack "-> closed"

parsePythonTime :: String -> LocalTime
parsePythonTime = parseTimeOrError True defaultTimeLocale "%Y-%m-%d&nbsp;%H:%M:%S"

periodLocalTime :: LocalTime -> LocalTime -> Integer
periodLocalTime a b = localTimeToInt a - localTimeToInt b where
  localTimeToInt i = (toModifiedJulianDay . localDay) i * 60*60*24 +
    (diffTimeToPicoseconds . timeOfDayToTime . localTimeOfDay) i where
      diffTimeToPicoseconds :: DiffTime -> Integer
      diffTimeToPicoseconds (MkDiffTime (MkFixed x)) = x

-- New changeset <a href="http://hg.python.org/lookup/810d70fb17a2">810d70fb17a2</a> by Serhiy Storchaka in branch 'default':
