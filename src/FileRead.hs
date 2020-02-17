import System.IO
import Data.List
import Data.List.Split
import Data.Typeable
import Control.Monad
import qualified Data.Text as Text
import qualified Data.Text.IO as Text


readFloat :: String -> Float
readFloat = read

readContents :: IO()
readContents = do
    contents <- readFile "statsNumbers.txt"
    let x = map readFloat . words $ contents
    print (show (typeOf x))

