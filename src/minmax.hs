--Finds the minimum, gets a list of Ints and returns an Int
getMin :: [Int] -> Maybe Int --Maybe is used incase an error occurs
getMin [] = Nothing --If the list/head is empty, nothing is returned
getMin [x] = Just x --If it consists of just one number
getMin (x:xs) = min x <$> getMin xs --Takes the function and functor that takes x and function that takes xs and returns a functor that contains cs

min :: Int -> Int -> Int --Function to compare the two numbers
min a b
    | a > b = b
    | a < b = a
    | a == b = a

getMax :: [Int] -> Maybe Int --Same as above but to find the max.
getMax [] = Nothing
getMax [x] = Just xs
getMax (x:xs) max x <$> getMax xs

max :: Int -> Int -> Int
max a b
    | a > b = a
    | a < b = b
    | a == b = a
