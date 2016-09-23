module Python
  (
  scrapePythonURL,
  ) where

import Control.Monad
import Debug.Trace
import Data.Maybe
import Data.UnixTime
import Data.ByteString.Char8 (pack)
import Data.ByteString (ByteString)
import Data.List
import Foreign.C.Types
import qualified Stats as S
import Stats (Stats)
import Text.HTML.Scalpel hiding (URL)

type URL = String
type PythonFieldSet = [String]
type PythonHistory = [String]
data PythonIssue = PythonIssue
  { pythonFieldSets :: [PythonFieldSet]
  , pythonHistories :: [PythonHistory]
  } deriving Show

scrapePythonURL :: URL -> IO (Maybe Stats)
scrapePythonURL url = fmap parsePythonIssue <$> scrapeURL url issue

issue :: Scraper String PythonIssue
issue = PythonIssue <$> fieldSets <*> histories

fieldSets :: Scraper String [PythonFieldSet]
fieldSets = chroots ("fieldset" // "table") $ texts "td"

histories :: Scraper String [PythonHistory]
histories = chroots ("table" @: [hasClass "table-striped"] // "tr") $ texts "td"

parsePythonIssue :: PythonIssue -> Stats
parsePythonIssue issue = S.Stats
  { S.period = udtSeconds $ diffUnixTime new old
  , S.priority = pythonFieldSets issue !! 1 !! 6
  , S.reopen = pred . length . filter (isInfixOf "-> closed") $ map (!! 3) hists
  }
    where
      hists = pythonHistories issue :: [PythonHistory]
      time = parsePythonTime . pack . (!! 0)
      new = time $ head hists
      old = time $ last hists

parsePythonTime :: ByteString -> UnixTime
parsePythonTime = parseUnixTime . pack $ "%Y-%m-%d&nbsp;%H:%M:%S"
