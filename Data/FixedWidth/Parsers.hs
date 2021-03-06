module Data.FixedWidth.Parsers(
  blank,
  Date(..),
  fixDate8, fixDate8Maybe,
  fixInt, fixIntLJ, fixIntRJ,
  fixText,
  ) where

import Prelude as P

import Control.Applicative
import Data.Aeson (toJSON, ToJSON)
import Data.Attoparsec.Text as AT
import Data.Char (isDigit, isSpace)
import qualified Data.Text as T
import Text.Printf (printf)

isDigitOrSpace :: Char -> Bool
isDigitOrSpace c = isSpace c || isDigit c

readIntRightJustified :: String -> Maybe Int
readIntRightJustified s =
  case P.dropWhile isSpace s of
  "" -> Nothing
  s' -> Just $ read s'

readIntLeftJustified :: String -> Maybe Int
readIntLeftJustified s =
  case P.takeWhile (not . isSpace) s of
  "" -> Nothing
  s' -> Just $ read s'

readIntStrip :: String -> Maybe Int
readIntStrip s =
  case P.takeWhile (not . isSpace) $ P.dropWhile isSpace s of
  "" -> Nothing
  s' -> Just $ read s'

fixInt' :: (String -> Maybe Int) -> Int -> Parser (Maybe Int)
fixInt' reader nDigits =
  fmap reader $ count nDigits (satisfy isDigitOrSpace)

fixInt :: Int -> Parser (Maybe Int)
fixInt = fixInt' readIntStrip

fixIntLJ :: Int -> Parser (Maybe Int)
fixIntLJ = fixInt' readIntLeftJustified

fixIntRJ :: Int -> Parser (Maybe Int)
fixIntRJ = fixInt' readIntRightJustified

data Date = Date {dYear :: Int,
                  dMonth :: Int,
                  dDay :: Int}

instance Show Date where
  show (Date y m d) = printf "%04d-%02d-%02d" y m d

instance ToJSON Date where
  toJSON date =
    toJSON $ show date

fixDate8 :: Parser Date
fixDate8 = do
  (Just year)  <- fixInt 4
  (Just month) <- fixInt 2
  (Just day)   <- fixInt 2
  return $ Date year month day

fixText :: Int -> Parser T.Text
fixText = AT.take

blank :: Int -> Parser ()
blank n = fmap (const ()) $ count n (satisfy isSpace)

fixOptional :: (Int -> Parser a) -> Int -> Parser (Maybe a)
fixOptional f n =
  fmap Just (f n) <|> fmap (const Nothing) (blank n)

fixDate8Maybe :: Parser (Maybe Date)
fixDate8Maybe = fixOptional (const fixDate8) 8
