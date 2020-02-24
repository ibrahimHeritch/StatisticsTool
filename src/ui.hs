import Data.Char
import Control.Monad
import FileRead
import Average
import FrequencyMap
import MinMax
import Data.Typeable
import Median
import System.IO
import Data.Foldable (for_)
import Data.Map

main = do
    putStrLn "-----Simple Statistics Calculator-----"
    putStrLn "What would you like to do?"
    putStrLn "a) Find the average."
    putStrLn "b) Find Min and Max."
    putStrLn "c) Find the Median."
    putStrLn "d) Find the Frequency List."
    putStrLn "e) Quit the program."

    choice <- getChar
    clear <- getChar --this is janky please don't delete
    let upperChoice = toUpper choice
    
    d<-getData "statsNumbers.txt"
    
    
    if upperChoice == 'A'
        then do
            putStrLn "Finding the average..."
            putStrLn(show(average d))
            main
    else if upperChoice == 'B'
        then do
            putStrLn "Finding the min and max..."
            putStrLn("min " ++show(getMin d))
            putStrLn("max " ++show(getMax d))
            main
    else if upperChoice == 'C'
        then do
            putStrLn "Finding the median..."
            putStrLn(show(median d))
            main
    else if upperChoice == 'D'
        then do
            putStrLn "Finding the frequency map..."
            for_ (toList (freqMap d)) $ \(q,a) -> putStrLn(show(q) ++ " " ++ show(a))
            main
    else if upperChoice == 'E'
        then do
            putStrLn "The program has ended."
    else putStrLn "Invalid input. Ending program."


