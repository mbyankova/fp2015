sumOfDivisors :: Int -> Int
sumOfDivisors number = iter 1 2
    where
        iter result current
            |current == number = result
            |(mod number current) == 0 = iter (result + current) (current + 1)
            |otherwise = iter result (current + 1)

interestingNumber :: Int -> Bool
interestingNumber number
    |number <= 1 = False
    |otherwise = number == (sumOfDivisors (sumOfDivisors number))

