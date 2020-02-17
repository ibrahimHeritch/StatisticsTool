import System.IO

main = do
    handle <- openFile "statsNumbers.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle