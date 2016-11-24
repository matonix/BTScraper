{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module JiraJson
  ( scrapeJiraRest
  , scrapeJira
  ) where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy as B
import           Data.Csv
import           Data.Maybe
import           Data.Text            (Text)
import qualified Data.Text            as T
import           GHC.Generics         (Generic)
import           Network.HTTP.Simple

data JiraIssue = JiraIssue
  { issueid    :: Text
  , summary    :: Text
  , issuetype  :: Text
  , status     :: Text
  , resolution :: Text
  , priority   :: Text
  , created    :: Text
  , updated    :: Text
  , resolved   :: Text
  , reopens    :: Int
  } deriving (Show, Generic)

instance FromRecord JiraIssue
instance ToRecord JiraIssue
instance ToNamedRecord JiraIssue
instance DefaultOrdered JiraIssue

-- url = "https://issues.apache.org/jira/rest/api/2/issue/LANG-747?expand=changelog"
prefix = "https://issues.apache.org/jira/rest/api/2/issue/LANG-"
suffix = "?expand=changelog"

scrapeJira :: IO ()
scrapeJira = mapM scrapeJiraRest [1..2] >>= B.putStrLn . encodeDefaultOrderedByName . catMaybes

scrapeJiraRest :: Int -> IO (Maybe JiraIssue)
scrapeJiraRest iid = do
  let url = prefix ++ show iid ++ suffix
  req <- parseRequest url
  res <- httpLBS req
  return . makeJiraIssue $ getResponseBody res

makeJiraIssue :: B.ByteString -> Maybe JiraIssue
makeJiraIssue json = JiraIssue
  <$> json ^? key "key" . _String
  <*> json ^? key "fields" . key "summary" . _String
  <*> json ^? key "fields" . key "issuetype" . key "name" . _String
  <*> json ^? key "fields" . key "status" . key "name" . _String
  <*> json ^? key "fields" . key "resolution" . key "name" . _String
  <*> json ^? key "fields" . key "priority" . key "name" . _String
  <*> json ^? key "fields" . key "created" . _String
  <*> json ^? key "fields" . key "updated" . _String
  <*> json ^? key "fields" . key "resolutiondate" . _String
  <*> Just (length . filter (== "Reopened") . concatMap
      (^.. key "items" . values . key "toString" . _String)
      $ json ^.. key "changelog" . key "histories" . values)
