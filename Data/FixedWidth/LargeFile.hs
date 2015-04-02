module Data.FixedWidth.LargeFile where

import Control.Applicative
import Data.Attoparsec.Text as StrictParse
import Data.Attoparsec.Text.Lazy as LazyParse
import qualified Data.Text as Strict
import qualified Data.Text.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as LazyBytes
import Data.Text.Lazy.Encoding (decodeUtf8)

fileToLazyText :: String -> IO Lazy.Text
fileToLazyText filename = fmap decodeUtf8 $ LazyBytes.readFile filename

strictLineNL :: Parser Strict.Text
strictLineNL = (StrictParse.takeTill isEndOfLine) <* endOfLine

strictLine :: Parser Strict.Text
strictLine = strictLineNL <|> StrictParse.takeText

chunkLazyText :: Parser Strict.Text -> Lazy.Text -> ([Strict.Text], Maybe String)
chunkLazyText chunkParser lazyText =
  if Lazy.null lazyText
  then ([], Nothing)
  else
    case (LazyParse.parse chunkParser lazyText) of
     LazyParse.Done rest' chunk ->
       let (chunks, failOpt) = chunkLazyText chunkParser rest'
       in (chunk : chunks, failOpt)
     failure@(LazyParse.Fail _ _ _) -> ([], Just $ show failure)

-- WARNING: You must discard the 2nd argument if you only intend to
-- parse part of the file.
chunkFile :: String -> (Parser Strict.Text) -> IO ([Strict.Text], Maybe String)
chunkFile filename parser = do
  lazyText <- fileToLazyText filename
  return $ chunkLazyText parser lazyText  
