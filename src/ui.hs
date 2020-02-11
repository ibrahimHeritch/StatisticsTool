import Data.Char
import Control.Monad

main = do
    putStrLn "-----Simple Statistics Calculator-----"
    putStrLn "What would you like to do?"
    putStrLn "a) Find the average."
    putStrLn "b) Find Min and Max."
    putStrLn "c) Find the Median."
    putStrLn "d) Quit the program."

    choice <- getChar
    let upperChoice = toUpper choice
    
    if upperChoice == 'A'
        then do
            putStrLn "Finding the average..."
    else if upperChoice == 'B'
        then do
            putStrLn "Finding the min and max..."
    else if upperChoice == 'C'
        then do
            putStrLn "Finding the median..."
    else if upperChoice == 'D'
        then do
            putStrLn "The program has ended."
    else putStrLn "Invalid input. Ending program."


