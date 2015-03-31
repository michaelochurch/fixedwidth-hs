{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)
import Data.FixedWidth (withFile, strictLine)
import Data.FixedWidth.Examples (defaultLineIterator)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  when (length args == 0) $ error "usage: \"quickparse\" <filename>"
  withFile (args !! 0) strictLine defaultLineIterator
