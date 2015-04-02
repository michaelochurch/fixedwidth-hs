{-# LANGUAGE OverloadedStrings #-}

module Data.FixedWidth.Examples where

import Data.FixedWidth.Parsers

import Data.Aeson
import Data.Attoparsec.Text as StrictText

import qualified Data.Text as T

data Entry = Entry {eDate :: Date,
                    eNames :: [T.Text],
                    eValue :: Int} deriving Show

parseEntry :: Parser Entry
parseEntry = do
  date <- fixDate8
  names <- count 4 (StrictText.take 4)
  (Just value) <- fixInt 3
  return $ Entry date names value

instance ToJSON Entry where
  toJSON (Entry date names value) =
    object ["date" .= date,
            "names" .= names,
            "value" .= value]
