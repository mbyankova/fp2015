containsDigit :: Integer -> Integer -> Bool
containsDigit n x
    |(n == 0) && (x == 0) = True
    |otherwise = iter n x
        where
            iter n x
                |n == 0 = False
                |mod n 10 == x = True
                |otherwise = iter (div n 10) x

containsDigits :: Integer -> Integer -> Bool
containsDigits container containee
    |containee == 0 = containsDigit container 0
    |otherwise = iter container containee
        where
            iter container containee
                |containee == 0 = True
                |otherwise = containsDigit container (mod containee 10) 
                && iter container (div containee 10)
