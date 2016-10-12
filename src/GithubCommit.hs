{-# LANGUAGE DeriveGeneric #-}

module GithubCommit
  ( linkGithubCommitToIssueCSV
  , linkGithubCommitToIssue
  ) where

import           Control.Monad
import           Csv
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Csv
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import           GHC.Generics               (Generic)
import           Text.HTML.Scalpel          hiding (URL)

data CommitIssue = CommitIssue
  { bugId       :: ByteString
  , oldCommitId :: ByteString
  , newCommitId :: ByteString
  , issueId     :: ByteString
  } deriving (Show,Generic)

instance FromRecord CommitIssue where
  parseRecord v = CommitIssue <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 0
instance ToRecord CommitIssue

linkGithubCommitToIssueCSV :: String -> FilePath -> FilePath -> IO ()
linkGithubCommitToIssueCSV prefix inCSV outCSV = do
  Right csv <- readCSV inCSV
  commitDescs <- mapM (scrapeGithubCommit prefix) $ toNewCommitIds csv
  writeCSV outCSV $ zipWith (\c i -> c {issueId = i}) (V.toList csv) $ map parseIssueId commitDescs

-- for test
linkGithubCommitToIssue :: String -> String -> IO ()
linkGithubCommitToIssue prefix commitId = do
  commitDescs <- scrapeGithubCommit prefix commitId
  print $ parseIssueId commitDescs
  -- print commitDescs

toNewCommitIds :: Vector CommitIssue -> [String]
toNewCommitIds = map (BS.unpack . newCommitId) . V.toList

scrapeGithubCommit :: String -> String -> IO (Maybe ByteString)
scrapeGithubCommit prefix commitId = scrapeURL (prefix ++ commitId) commit

commit :: Scraper ByteString ByteString
commit = BS.append <$> commitTitle <*> commitDesc where
  commitTitle = text ("p" @: [hasClass "commit-title"])
  commitDesc = text ("div" @: [hasClass "commit-desc"])

parseIssueId :: Maybe ByteString -> ByteString
parseIssueId (Just str) = pickIssueId . dropWhile notFoundIssue $ mywords str where
  mywords = BS.splitWith (flip elem "\n\r\t ")
  notFoundIssue str = str /= (BS.pack "issue") && str /= (BS.pack "Issue")
  pickIssueId (_:a:_) = maybe BS.empty (BS.pack . show . fst) $ BS.readInt a
  pickIssueId _ = BS.empty
parseIssueId Nothing = BS.empty
