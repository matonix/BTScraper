{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Stats
  ( Stats(..)
  , writeStats
  ) where

import           Data.Binary
import           Data.ByteString (ByteString)
import           Data.Csv
import           Data.Int
import           Foreign.C.Types
import           GHC.Generics    (Generic)

data Stats = Stats
  { issueNum :: Int
  , period   :: Int64
  , priority :: ByteString
  , reopen   :: Int
  } deriving (Show,Generic)

instance ToNamedRecord Stats where
  toNamedRecord (Stats issueNum period priority reopen) = namedRecord
    [ "issueNum" .= issueNum
    , "period"   .= period
    , "priority" .= priority
    , "reopen"   .= reopen
    ]

instance DefaultOrdered Stats where
  headerOrder _ = header ["issueNum", "period", "priority", "reopen"]

instance Binary Stats

writeStats :: [Stats] -> IO ()
writeStats = print . encodeDefaultOrderedByName
