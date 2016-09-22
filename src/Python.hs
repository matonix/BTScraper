module Python
  (
  scrapePythonURL,
  ) where

import Text.HTML.Scalpel hiding (URL)
import Debug.Trace
import Data.Maybe
import Data.UnixTime
import Data.ByteString.Char8 (pack)
import Data.ByteString (ByteString)
import Foreign.C.Types
import Stats

data PythonHistory
  = PythonHistory {
    date    :: ByteString,
    user    :: ByteString,
    action  :: ByteString,
    args    :: ByteString
  } deriving (Show, Eq)

scrapePythonURL :: String -> IO Stats
scrapePythonURL url = parsePythonHistory . fromJust <$> scrapeURL url histories

histories :: Scraper String [PythonHistory]
histories = chroots ("table" @: [hasClass "table-striped"] // "tr") history

history :: Scraper String PythonHistory
history = do
  tds <- map pack <$> innerHTMLs "td"
  return PythonHistory {
    date    = head tds,
    user    = tds !! 1,
    action  = tds !! 2,
    args    = tds !! 3
  }

parsePythonHistory :: [PythonHistory] -> Stats
parsePythonHistory hist = Stats {
    period = udtSeconds $ diffUnixTime new old,
    priority = "normal",
    reopen = 0
  } where
    new = parsePythonTime . date . head $ hist
    old = parsePythonTime . date . last $ hist

parsePythonTime :: ByteString -> UnixTime
parsePythonTime = parseUnixTime . pack $ "%Y-%m-%d&nbsp;%H:%M:%S"
