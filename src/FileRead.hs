import System.IO
import Data.List
import Data.Typeable
import Control.Monad
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Median


readFloat :: String -> Float
readFloat = read


getData:: String -> IO([Float])
getData fileName= do
    contents <- readFile fileName
    let x = map readFloat.words $ contents
    return x

main = do
      d<-getData "statsNumbers.txt"
      putStr ("The Data is :"++show (d)++"\n")
      putStr("Median is "++show(median d)++"\n")
      putStr(show (typeOf d))
