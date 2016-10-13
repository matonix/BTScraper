module Jira
    ( scrapeJiraURL
    ) where

import           Control.Monad
import           Csv
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Text.HTML.Scalpel

data JiraIssue = JiraIssue
  { jiraPriority    :: ByteString
  , jiraTransitions :: [JiraTransition]
  } deriving Show

-- data JiraTransition = JiraTransition
--   { transitionPair     :: ByteString
--   , timeInSourceStatus :: ByteString
--   , executionTimes     :: ByteString
--   , lastExecuter       :: ByteString
--   , lastExecutionDate  :: ByteString
--   } deriving Show

type JiraTransition = ByteString

scrapeJiraURL :: URL -> IO (Maybe JiraIssue)
scrapeJiraURL url = scrapeURL url scrapeIssue

scrapeIssue :: Scraper ByteString JiraIssue
scrapeIssue =  JiraIssue
           <$> scrapePriority
           <*> scrapeJiraTransitions

scrapePriority :: Scraper ByteString ByteString
scrapePriority = attr "alt" $ "span" @: ["id" @= "priority-val"] // "img"

scrapeJiraTransitions :: Scraper ByteString [JiraTransition]
scrapeJiraTransitions = chroots ("div" @: [hasClass "issuePanelContainer"] // "table" // "tr") scrapeJiraTransition
  where
    scrapeJiraTransition :: Scraper ByteString JiraTransition
    scrapeJiraTransition = innerHTML "td"
    -- scrapeJiraTransition =  JiraTransition
    --                     <$> text "td"
    --                     <*> text "td"
    --                     <*> text "td"
    --                     <*> text "td"
    --                     <*> text "td"
