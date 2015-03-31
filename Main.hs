{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import Data.Aeson as Aeson
import Data.Attoparsec.Text as Parse
--import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (isDigit, isSpace)
import qualified Data.Text as T
--import Data.Text.Lazy.Encoding (decodeUtf8)
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
  names <- count 4 (Parse.take 4)
  value <- fixInt 3
  endOfLine
  return $ Entry eDate names value

parseLines :: Parser a -> Parser [a]
parseLines parser = many parser

instance ToJSON Date where
  toJSON date =
    toJSON $ show date

instance ToJSON Entry where
  toJSON (Entry date names value) =
    object ["date" .= date,
            "names" .= names,
            "value" .= value]

-- runParseFile :: String -> IO ()
-- runParseFile filename = do
--   bytes <- BL.readFile filename
--   -- TODO: we don't want to load the whole file. (We need to use laziness!)
--   let result = parseOnly (many entry) (decodeUtf8 bytes)
--   case result of
--    -- TODO: this requires parsing the whole thing before doing anything. That's bad.
--    Left errorMsg -> error errorMsg
--    Right entries -> forM_ entries $ \e ->
--      BL.putStrLn $ encode e

-- main :: IO ()
-- main = do
--   args <- getArgs
--   when (length args == 0) $ error "usage: \"quickparse\" <filename>"
--   runParseFile $ args !! 0
