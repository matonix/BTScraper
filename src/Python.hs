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

data PythonBTS = PythonBTS
  { pythonPriority :: [String]
  , pythonHistories :: [PythonHistory]
  } deriving Show

data PythonHistory = PythonHistory
  { date    :: String
  , user    :: String
  , action  :: String
  , args    :: String
  } deriving Show

scrapePythonURL :: String -> IO (Maybe Stats)
scrapePythonURL url = fmap parsePythonBTS <$> scrapeURL url bts

bts :: Scraper String PythonBTS
bts = PythonBTS <$> priority <*> histories

priority :: Scraper String [String]
priority = chroots ("fieldset" // "table") priority'
  where
    priority' :: Scraper String String
    priority' = do
      cols <- mfilter ((==4) . length) . texts $ "tr"
      return $ cols !! 3


histories :: Scraper String [PythonHistory]
histories = chroots ("table" @: [hasClass "table-striped"] // "tr") history
  where
    history :: Scraper String PythonHistory
    history = do
      tds <- texts "td"
      return PythonHistory
        { date    = tds !! 0
        , user    = tds !! 1
        , action  = tds !! 2
        , args    = tds !! 3
        }

parsePythonBTS :: PythonBTS -> Stats
parsePythonBTS bts = S.Stats
  { S.period = udtSeconds $ diffUnixTime new old
  , S.priority = (!! 1) . words . head $ pythonPriority bts
  , S.reopen = pred . length . filter (isPrefixOf "-> closed") . tails $ concatMap args hists
  }
    where
      hists = pythonHistories bts :: [PythonHistory]
      new = parsePythonTime . pack . date . head $ hists
      old = parsePythonTime . pack . date . last $ hists

parsePythonTime :: ByteString -> UnixTime
parsePythonTime = parseUnixTime . pack $ "%Y-%m-%d&nbsp;%H:%M:%S"
