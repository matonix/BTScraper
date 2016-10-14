{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Stats
  ( Stats(..)
  , writeStats
  , printStats
  ) where

import           Data.Binary
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Csv
import           Data.Int
import           Data.Time
import           Foreign.C.Types
import           GHC.Generics               (Generic)

data Stats = Stats
  { issueIds :: Int
  , period   :: Int
  , priority :: ByteString
  , reopen   :: Int
  , commits  :: [ByteString]
  } deriving (Show,Generic)

instance ToNamedRecord Stats where
  toNamedRecord (Stats issueId period priority reopen commits) = namedRecord
    [ "issueId" .= issueId
    , "period"   .= period
    , "priority" .= priority
    , "reopen"   .= reopen
    , "commits"  .= BS.intercalate " " commits
    ]

instance DefaultOrdered Stats where
  headerOrder _ = header ["issueId", "period", "priority", "reopen", "commits"]

instance Binary Stats

writeStats :: FilePath -> [Stats] -> IO ()
writeStats f = BL.writeFile f . encodeDefaultOrderedByName

printStats :: [Stats] -> IO ()
printStats = print . encodeDefaultOrderedByName
