import System.IO
import Data.List.Split
import Data.Typeable


readFloat :: String -> IO Float
readFloat = read

getInput :: FilePath -> [Float]
getInput input = do
    contents <- readFile "statsNumbers.txt" 
    l <- sequence contents 
    map readFloat . words $ l
    