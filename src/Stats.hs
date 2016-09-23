module Stats
    (
    Stats(..)
    ) where

import Foreign.C.Types
import Data.ByteString (ByteString)

data Stats = Stats {
  period :: CTime,
  priority :: ByteString,
  reopen :: Int
} deriving (Show, Eq)
