module Python
  ( scrapePythonURL
  , scrapePythonCSV
  ) where

import           Control.Monad
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Csv
<<<<<<< HEAD
import           Data.Maybe                 (catMaybes, fromJust, mapMaybe)
import           Data.UnixTime
=======
import           Data.Fixed
import           Data.Int
import           Data.Maybe                 (catMaybes, fromJust, mapMaybe)
import           Data.Time
>>>>>>> issue1
import qualified Data.Vector                as V
import           Debug.Trace
import           Foreign.C.Types
import           Stats
import           Text.HTML.Scalpel          hiding (URL)

scrapePythonCSV :: FilePath -> IO [Stats]
scrapePythonCSV csvFile = do
  Right csv <- parseCSV csvFile
<<<<<<< HEAD
  catMaybes <$> mapM scrapePythonURL (getIssueNums csv)
=======
  catMaybes <$> mapM scrapePythonIssue (getIssueNums csv)
>>>>>>> issue1

parseCSV :: FilePath -> IO (Either String Csv)
parseCSV csvFile = decode NoHeader <$> BL.readFile csvFile

getIssueNums :: Csv -> [Int]
getIssueNums = map fst . mapMaybe (BS.readInt . (V.! 1)) . V.toList

-- for scraping python issue

type PythonFieldSet = [ByteString]
<<<<<<< HEAD
type PythonHistory = [ByteString]
data PythonIssue = PythonIssue
  { pythonFieldSets :: [PythonFieldSet]
  , pythonHistories :: [PythonHistory]
  } deriving Show

scrapePythonURL :: Int -> IO (Maybe Stats)
scrapePythonURL issueNum =
=======
-- type PythonMessage  = [ByteString]
type PythonHistory  = [ByteString]

data PythonIssue = PythonIssue
  { pythonFieldSets :: [PythonFieldSet]
  -- , pythonMessages  :: [PythonMessage]
  , pythonHistories :: [PythonHistory]
  } deriving Show

scrapePythonURL :: String -> IO (Maybe PythonIssue)
scrapePythonURL url = scrapeURL url issue

scrapePythonIssue :: Int -> IO (Maybe Stats)
scrapePythonIssue issueNum =
>>>>>>> issue1
  fmap (parsePythonIssue issueNum) <$> scrapeURL url issue where
    url = "http://bugs.python.org/issue" ++ show issueNum

issue :: Scraper ByteString PythonIssue
<<<<<<< HEAD
=======
-- issue = PythonIssue <$> fieldSets <*> messages <*> histories
>>>>>>> issue1
issue = PythonIssue <$> fieldSets <*> histories

fieldSets :: Scraper ByteString [PythonFieldSet]
fieldSets = chroots ("fieldset" // "table") $ texts "td"

<<<<<<< HEAD
=======
-- messages :: Scraper ByteString [PythonMessage]
-- messages = chroots ("table" @: [hasClass "messages"])

>>>>>>> issue1
histories :: Scraper ByteString [PythonHistory]
histories = chroots ("table" @: [hasClass "table-striped"] // "tr") $ texts "td"

parsePythonIssue :: Int -> PythonIssue -> Stats
parsePythonIssue inum issue = Stats
  { issueNum = inum
<<<<<<< HEAD
  , period = (\(CTime a) -> a) . udtSeconds $ diffUnixTime new old
=======
  , period = init . show $ diffUTCTime new old
>>>>>>> issue1
  , priority = pythonFieldSets issue !! 1 !! 6
  , reopen = pred . length . filter containsClosed $ map (!! 3) hists
  } where
    hists = pythonHistories issue :: [PythonHistory]
<<<<<<< HEAD
    time = parsePythonTime . (!! 0)
=======
    time = parsePythonTime . BS.unpack . (!! 0)
>>>>>>> issue1
    new = time $ head hists
    old = time $ last hists
    containsClosed = BS.isInfixOf $ BS.pack "-> closed"

<<<<<<< HEAD
parsePythonTime :: ByteString -> UnixTime
parsePythonTime = parseUnixTime $ BS.pack "%Y-%m-%d&nbsp;%H:%M:%S"
=======
parsePythonTime :: String -> UTCTime
parsePythonTime = parseTimeOrError True defaultTimeLocale "%F\160\&%T"
>>>>>>> issue1

-- New changeset <a href="http://hg.python.org/lookup/810d70fb17a2">810d70fb17a2</a> by Serhiy Storchaka in branch 'default':
