import Control.Applicative (many, (<*))
import Control.Monad (when)
import Data.Attoparsec.Text as Parse
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString as B
import System.Environment (getArgs)

-- Have to:
--    1. Parse a fixed-width file.
--    2. Render useful JSON.

-- class JSONable a where
--   toJSON :: a -> JSON

data Date = Date {dYear :: Int,
                  dMonth :: Int,
                  dDay :: Int} deriving Show

data Entry = Entry {eDate :: Date,
                    eNames :: [Text],
                    eValue :: Int} deriving Show

fixInt :: Int -> Parser Int
fixInt n = fmap read $ count n digit

date :: Parser Date
date = do
  year  <- fixInt 4
  month <- fixInt 2
  day   <- fixInt 2
  return $ Date year month day

entry = do
  eDate <- date
  names <- count 4 (Parse.take 4)
  value <- fixInt 3
  return $ Entry eDate names value

-- TODO : many doesn't seem to go past one entry. Figure this out.
parseLines :: Parser a -> Parser [a]
parseLines parser = many $ parser

runParseFile :: String -> IO ()
runParseFile filename = do
  bytes <- B.readFile filename
  let result = parseOnly (parseLines entry) (decodeUtf8 bytes)
  print result

main :: IO ()
main = do
  args <- getArgs
  when (length args == 0) $ error "usage: \"quickparse\" <filename>"
  runParseFile $ args !! 0
