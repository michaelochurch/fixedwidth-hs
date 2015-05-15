module Data.FixedWidth.LargeFile where

import qualified Data.Text as StrictText
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.IO as LazyTextIO
import System.IO (Handle, IOMode(..), openFile)

hGetLines :: Handle -> IO [StrictText.Text]
hGetLines handle = do
  lazyText <- LazyTextIO.hGetContents handle
  return $ map LazyText.toStrict (LazyText.lines lazyText)

getLines :: String -> IO [StrictText.Text]
getLines filename = openFile filename ReadMode >>= hGetLines
