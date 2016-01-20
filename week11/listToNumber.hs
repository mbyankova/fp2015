listToNumber :: [Int] -> Int
listToNumber [] = 0
listToNumber (x:xs) = x*(10^(length xs)) + (listToNumber xs)