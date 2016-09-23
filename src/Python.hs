module Python
  (
  scrapePythonURL,
  ) where

import Control.Monad
import Debug.Trace
import Data.Maybe
import Data.UnixTime
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Foreign.C.Types
import qualified Stats as S
import Stats (Stats)
import Text.HTML.Scalpel hiding (URL)
--
-- type CSV = [[ByteString]]
--
-- parseCSV :: FilePath -> IO CSV
-- parseCSV ::

-- for scraping python issue

type URL = String
type PythonFieldSet = [ByteString]
type PythonHistory = [ByteString]
data PythonIssue = PythonIssue
  { pythonFieldSets :: [PythonFieldSet]
  , pythonHistories :: [PythonHistory]
  } deriving Show

scrapePythonURL :: URL -> IO (Maybe Stats)
scrapePythonURL url = fmap parsePythonIssue <$> scrapeURL url issue

issue :: Scraper ByteString PythonIssue
issue = PythonIssue <$> fieldSets <*> histories

fieldSets :: Scraper ByteString [PythonFieldSet]
fieldSets = chroots ("fieldset" // "table") $ texts "td"

histories :: Scraper ByteString [PythonHistory]
histories = chroots ("table" @: [hasClass "table-striped"] // "tr") $ texts "td"

parsePythonIssue :: PythonIssue -> Stats
parsePythonIssue issue = S.Stats
  { S.period = udtSeconds $ diffUnixTime new old
  , S.priority = pythonFieldSets issue !! 1 !! 6
  , S.reopen = pred . length . filter (BS.isInfixOf $ BS.pack "-> closed") $ map (!! 3) hists
  }
    where
      hists = pythonHistories issue :: [PythonHistory]
      time = parsePythonTime . (!! 0)
      new = time $ head hists
      old = time $ last hists

parsePythonTime :: ByteString -> UnixTime
parsePythonTime = parseUnixTime $ BS.pack "%Y-%m-%d&nbsp;%H:%M:%S"
