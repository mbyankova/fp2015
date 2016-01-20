isPrime :: Int -> Bool
isPrime n
    | n <= 1 = False
    | otherwise = isPrime' 2 n
        where
            isPrime' current n
                | current == n = True
                | mod n current == 0 = False
                | otherwise = isPrime' (current + 1) n

truncatablePrime :: Int -> Bool
truncatablePrime x
    | x <= 1 = False
    |otherwise = truncatablePrime' x
	where
	    truncatablePrime' x
		| (isPrime x) == True = truncatablePrime' (div x 10)
		| x == 0 = True
		| otherwise = False