{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import Data.Aeson as Aeson
import Data.Attoparsec.Text as StrictText
import Data.Attoparsec.Text.Lazy as LazyText
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (isDigit, isSpace)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.ByteString as B
import System.Environment (getArgs)
import Text.Printf (printf)

data Date = Date {dYear :: Int,
                  dMonth :: Int,
                  dDay :: Int}

instance Show Date where
  show (Date y m d) = printf "%04d-%02d-%02d" y m d

data Entry = Entry {eDate :: Date,
                    eNames :: [T.Text],
                    eValue :: Int} deriving Show

isDigitOrSpace :: Char -> Bool
isDigitOrSpace c = (isDigit c) || (isSpace c)

fixInt :: Int -> Parser Int
fixInt n = fmap (read . dropWhile isSpace) $ count n (satisfy isDigitOrSpace)

date :: Parser Date
date = do
  year  <- fixInt 4
  month <- fixInt 2
  day   <- fixInt 2
  return $ Date year month day

entry :: Parser Entry
entry = do
  eDate <- date
  names <- count 4 (StrictText.take 4)
  value <- fixInt 3
  return $ Entry eDate names value

instance ToJSON Date where
  toJSON date =
    toJSON $ show date

instance ToJSON Entry where
  toJSON (Entry date names value) =
    object ["date" .= date,
            "names" .= names,
            "value" .= value]


fileToLazyText :: String -> IO TL.Text
fileToLazyText filename = fmap decodeUtf8 $ BL.readFile filename

strictLine :: Parser T.Text
strictLine = (StrictText.takeTill isEndOfLine) <* endOfLine

withFile :: String -> (Parser a) -> (a -> IO ()) -> IO ()
withFile filename parser action = do
  text <- fileToLazyText filename
  let loop rest = if (TL.null rest)
                  then return ()
                  else
                    case (LazyText.parse parser rest) of
                    LazyText.Done rest' a -> (action a >> loop rest')
                    _ -> error "failed parse"
  loop text

lineIterator :: Parser a -> IO () -> (a -> IO ()) -> T.Text -> IO ()
lineIterator parser fail succeed text =
  case (parseOnly parser text) of
    Left _ -> fail
    Right a -> succeed a

defaultLineIterator :: T.Text -> IO ()
defaultLineIterator =
  lineIterator entry (putStrLn "Unparseable line.")
               (BL.putStrLn . encode)

main :: IO ()
main = do
  args <- getArgs
  when (length args == 0) $ error "usage: \"quickparse\" <filename>"
  withFile (args !! 0) strictLine defaultLineIterator
