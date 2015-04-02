import Control.Monad (when)
import Data.Aeson
import qualified Data.Attoparsec.Text as Parse
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.FixedWidth.Examples
import Data.FixedWidth.LargeFile (chunkFile, strictLine)
import qualified Data.Text as Strict
import System.Environment (getArgs)

getEntry :: Strict.Text -> Entry
getEntry text =
  case (Parse.parseOnly parseEntry text) of
    Right entry -> entry
    Left  _       -> error "shouldn't happen"

main :: IO ()
main = do
  args <- getArgs
  when (length args == 0) $ error "usage: \"quickparse\" <filename>"
  (fileLines, _) <- chunkFile (args !! 0) strictLine
  mapM_ (ByteString.putStrLn . encode . getEntry) fileLines
