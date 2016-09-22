module Stats
    (
    Stats(..)
    ) where

import Foreign.C.Types

data Stats = Stats {
  period :: CTime,
  priority :: String,
  reopen :: Int
} deriving (Show, Eq)
