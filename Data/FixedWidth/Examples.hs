{-# LANGUAGE OverloadedStrings #-}

module Data.FixedWidth.Examples where

import Data.Aeson
import Data.Attoparsec.Text as StrictText
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Char (isDigit, isSpace)
import Data.FixedWidth (lineIterator)
import qualified Data.Text as T
import Text.Printf (printf)

data Date = Date {dYear :: Int,
                  dMonth :: Int,
                  dDay :: Int}

data Month = Month {mYear :: Int,
                    mMonth :: Int}

instance Show Date where
  show (Date y m d) = printf "%04d-%02d-%02d" y m d

data Entry = Entry {eDate :: Date,
                    eNames :: [T.Text],
                    eValue :: Int} deriving Show

isDigitOrSpace :: Char -> Bool
isDigitOrSpace c = (isDigit c) || (isSpace c)

fixInt :: Int -> Parser Int
fixInt n = fmap (read . dropWhile isSpace) $ count n (satisfy isDigitOrSpace)

fixDate8 :: Parser Date
fixDate8 = do
  year  <- fixInt 4
  month <- fixInt 2
  day   <- fixInt 2
  return $ Date year month day

entry :: Parser Entry
entry = do
  date <- fixDate8
  names <- count 4 (StrictText.take 4)
  value <- fixInt 3
  return $ Entry date names value

instance ToJSON Date where
  toJSON date =
    toJSON $ show date

instance ToJSON Entry where
  toJSON (Entry date names value) =
    object ["date" .= date,
            "names" .= names,
            "value" .= value]

defaultLineIterator :: T.Text -> IO ()
defaultLineIterator =
  lineIterator entry (putStrLn "Unparseable line.")
               (BLC.putStrLn . encode)
