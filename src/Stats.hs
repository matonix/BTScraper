module Stats
  ( Stats(..)
  ) where

import Foreign.C.Types
import Data.ByteString (ByteString)

data Stats = Stats
  { issueNum  :: Int
  , period    :: CTime
  , priority  :: ByteString
  , reopen    :: Int
  } deriving Show
