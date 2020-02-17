import System.IO

readLines :: FilePath -> IO [String]
readLines :: fmap lines . readFile
makeInteger :: [String]  -> [Int]
makeInteger :: map read

main = do
    content <- readLines "statNumbers.txt"
    return (radix $ makeInteger content)