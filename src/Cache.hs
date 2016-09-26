{-# LANGUAGE BangPatterns #-}

module Cache
  ( scrapeWithCache
  ) where

import           Control.Exception
import           Data.Binary
import           Debug.Trace
import           Stats
import           System.IO

{-
 - Statsのキャッシュファイルを開き、
 - 開くことに成功したらそれを標準出力(consume)し、
 - 開くことに失敗したらスクレイパーを呼び、キャッシュし、標準出力する
 -}
scrapeWithCache :: FilePath -> (FilePath  -> IO [Stats]) -> ([Stats] -> IO a) -> IO a
scrapeWithCache file scraper consumer =
  catch (doDecodeFile cacheFile) doCache >>= consumer where
    cacheFile = file ++ ".statscache"
    doCache :: IOException -> IO [Stats]
    doCache e = do
      traceIO "cache not found :("
      traceIO "now scraping..."
      stats <- scraper file
      traceIO "done."
      traceIO "now caching..."
      encodeFile cacheFile stats
      traceIO $ "done. -> " ++ cacheFile
      return stats
    doDecodeFile :: FilePath -> IO [Stats]
    doDecodeFile f = do
      result <- decodeFileOrFail f
      case result of
        Right x -> do
            traceIO "cache found :)"
            return x
        Left (_,str) -> error str
