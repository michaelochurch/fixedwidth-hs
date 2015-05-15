import Control.Monad (when)
import Data.Aeson
import qualified Data.Attoparsec.Text as Parse
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.FixedWidth.Examples
import Data.FixedWidth.LargeFile
import qualified Data.Text as Strict
import System.Environment (getArgs)

-- TODO: the Entry-related stuff belongs in (a/) separate testing
-- module(s).

getEntry :: Strict.Text -> Entry
getEntry text =
  case (Parse.parseOnly parseEntry text) of
    Right entry   -> entry
    Left  _       -> error "shouldn't happen"

-- Right now, the function of this main is to verify the absence of a
-- memory leak in LargeFile.
main :: IO ()
main = do
  args <- getArgs
  when (length args == 0) $ error "this program requires a filename"
  let filename = args !! 0
  fmap length (getLines filename) >>= print
