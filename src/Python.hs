module Python
  ( scrapePythonURL
  , scrapePythonCsv
  , parseCsv
  ) where

import Control.Monad
import Data.Csv
import Data.Maybe (mapMaybe, fromJust)
import Data.UnixTime
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Vector as V
import Debug.Trace
import Foreign.C.Types
import Stats
import Text.HTML.Scalpel hiding (URL)

scrapePythonCsv :: FilePath -> IO [Maybe Stats]
scrapePythonCsv csvFile = do
  Right csv <- parseCsv csvFile
  mapM scrapePythonURL $ getIssueNums csv

parseCsv :: FilePath -> IO (Either String Csv)
parseCsv csvFile = decode NoHeader <$> BL.readFile csvFile

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
  fmap (parsePythonIssue issueNum) <$> scrapeURL url issue
  where
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
  , period = udtSeconds $ diffUnixTime new old
  , priority = pythonFieldSets issue !! 1 !! 6
  , reopen = pred . length . filter (BS.isInfixOf $ BS.pack "-> closed") $ map (!! 3) hists
  }
    where
      hists = pythonHistories issue :: [PythonHistory]
      time = parsePythonTime . (!! 0)
      new = time $ head hists
      old = time $ last hists

parsePythonTime :: ByteString -> UnixTime
parsePythonTime = parseUnixTime $ BS.pack "%Y-%m-%d&nbsp;%H:%M:%S"
