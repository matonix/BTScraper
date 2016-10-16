{-# LANGUAGE DeriveGeneric #-}

module GithubCommit
  ( linkGithubCommitToIssueCSV
  , linkGitlogCommitToIssueCSV
  , CommitIssue(..)
  ) where

import           Control.Monad
import           Csv
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Char                  (isDigit)
import           Data.Csv
import           Data.List                  (find)
import           Data.Maybe                 (fromMaybe)
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import           GHC.Generics               (Generic)
import           Text.HTML.Scalpel          hiding (URL)

data Commit = Commit
  { bugIdC       :: ByteString
  , oldCommitIdC :: ByteString
  , newCommitIdC :: ByteString
  } deriving (Show,Generic)

data CommitIssue = CommitIssue
  { bugId       :: ByteString
  , oldCommitId :: ByteString
  , newCommitId :: ByteString
  , issueId     :: ByteString
  } deriving (Show,Generic)

instance FromRecord Commit
instance ToRecord Commit

instance FromRecord CommitIssue
instance ToRecord CommitIssue

-- ↓broken codes

linkGithubCommitToIssueCSV :: String -> FilePath -> FilePath -> IO ()
linkGithubCommitToIssueCSV prefix inCSV outCSV = do
  Right csv <- readCSV inCSV
  commitDescs <- mapM (scrapeGithubCommit prefix) $ toNewCommitIds csv
  writeCSV outCSV $ zipWith (\c i -> c {issueId = i}) (V.toList csv) $ map parseIssueId commitDescs

linkGitlogCommitToIssueCSV :: FilePath -> FilePath -> FilePath -> IO ()
linkGitlogCommitToIssueCSV logFile inCSV outCSV = do
  Right csv <- readCSV inCSV
  logs <- map BS.words . BS.lines <$> BS.readFile logFile
  let issueIds = map (parseGitLog logs) . map newCommitId . V.toList $ csv
  writeCSV outCSV $ zipWith (\c i -> c {issueId = i}) (V.toList csv) issueIds

-- for test
-- linkGithubCommitToIssue :: String -> String -> IO ()
-- linkGithubCommitToIssue prefix commitId = do
--   commitDescs <- scrapeGithubCommit prefix commitId
--   print $ parseIssueId commitDescs

toNewCommitIds :: Vector CommitIssue -> [String]
toNewCommitIds = map (BS.unpack . newCommitId) . V.toList

scrapeGithubCommit :: String -> String -> IO (Maybe ByteString)
scrapeGithubCommit prefix commitId = scrapeURL (prefix ++ commitId) commit

commit :: Scraper ByteString ByteString
commit = BS.append <$> commitTitle <*> commitDesc where
  commitTitle = text ("p" @: [hasClass "commit-title"])
  commitDesc = text ("div" @: [hasClass "commit-desc"])

-- for Commons-lang
-- parseGitLog :: [[ByteString]] -> ByteString -> ByteString
-- parseGitLog logs commitId = pickIssueId . find lang . fromMaybe [] $ find (\a -> head a == commitId) logs where
--   lang = BS.isInfixOf (BS.pack "LANG-")
--   pickIssueId a = readIntEmpty . BS.filter isDigit $ maybeEmpty a

-- for Commons-math
parseGitLog :: [[ByteString]] -> ByteString -> ByteString
parseGitLog logs commitId = pickIssueId . find lang . fromMaybe [] $ find (\a -> head a == commitId) logs where
  lang = BS.isInfixOf (BS.pack "MATH-")
  pickIssueId a = readIntEmpty . BS.filter isDigit $ maybeEmpty a


-- for Joda-time
parseIssueId :: Maybe ByteString -> ByteString
parseIssueId str = pickIssueId . find sharp . mywords $ maybeEmpty str where
  mywords = BS.splitWith (flip elem "\n\r\t ")
  sharp str = BS.length str >= 2 && BS.head str == '#'
  pickIssueId a = readIntEmpty . BS.tail $ maybeEmpty a

-- for Closure-compiler
-- parseIssueId :: Maybe ByteString -> ByteString
-- parseIssueId (Just str) = pickIssueId . dropWhile notFoundIssue $ mywords str where
--   mywords = BS.splitWith (flip elem "\n\r\t ")
--   notFoundIssue str = str /= (BS.pack "issue") && str /= (BS.pack "Issue")
--   pickIssueId (_:a:_) = maybe BS.empty (BS.pack . show . fst) $ BS.readInt a
--   pickIssueId _ = BS.empty
-- parseIssueId Nothing = BS.empty

maybeEmpty :: Maybe ByteString -> ByteString
maybeEmpty = fromMaybe BS.empty

readIntEmpty :: ByteString -> ByteString
readIntEmpty = maybe BS.empty (BS.pack . show . fst) . BS.readInt
