module MinMax where
--Finds the minimum, gets a list of Ints and returns an Int
getMin :: [Int] -> Maybe Int --Maybe is used incase an error occurs
getMin [] = Nothing --If the list/head is empty, nothing is returned
getMin [x] = Just x --If it consists of just one number
getMin (x:xs) = mins x <$> getMin xs --Takes the function and functor that takes x and function that takes xs and returns a functor that contains cs

mins :: Int -> Int -> Int --Function to compare the two numbers
mins a b
    | a > b = b
    | a < b = a
    | a == b = a

getMax :: [Int] -> Maybe Int --Same as above but to find the max.
getMax [] = Nothing
getMax [x] = Just x
getMax (x:xs) = maxi x <$> getMax xs

maxi :: Int -> Int -> Int
maxi a b
    | a > b = a
    | a < b = b
    | a == b = a