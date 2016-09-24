{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Stats
  ( Stats(..)
  , writeStats
  , printStats
  ) where

import           Data.Binary
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
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

writeStats :: FilePath -> [Stats] -> IO ()
writeStats f = BL.writeFile f . encodeDefaultOrderedByName

printStats :: [Stats] -> IO ()
printStats = print . encodeDefaultOrderedByName
