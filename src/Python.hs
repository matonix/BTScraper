module Python
  (
  scrapePythonURL
  ) where

import Text.HTML.Scalpel hiding (URL)
import Debug.Trace

data PythonHistory
  = PythonHistory {
    date    :: String,
    user    :: String,
    action  :: String,
    args    :: String
  } deriving (Show, Eq)

scrapePythonURL :: String -> IO (Maybe [PythonHistory])
scrapePythonURL url = scrapeURL url histories

histories :: Scraper String [PythonHistory]
histories = chroots ("table" @: [hasClass "table-striped"] // "tr") history

history :: Scraper String PythonHistory
history = do
  tds <- innerHTMLs $ "td"
  return $ PythonHistory {
    date    = tds !! 0,
    user    = tds !! 1,
    action  = tds !! 2,
    args    = tds !! 3
  }
--
-- instance Show Item where
--   show (SubmittedItem   author title url) =
--     printf "%s: %s (%s)" author title url
--   show (UnsubmittedItem author comment) =
--     printf "%s: %s" author comment
