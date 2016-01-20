productOfDigits :: Int -> Int
productOfDigits 0 = 0
productOfDigits n = iter n 1
    where 
        iter n result
            |n == 0 = result
            |otherwise = iter (div n 10) (result * mod n 10)