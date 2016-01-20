occurrences :: [Int] -> [Int] -> [Int]
occurrences [] _ = []
--occurrences (x:xs) ys = (numberOfOccurences x ys) : (occurrences xs ys)
occurrences xs ys = [(numberOfOccurences z ys) | z <- xs]
 
numberOfOccurences :: Int -> [Int] -> Int
numberOfOccurences x [] = 0
numberOfOccurences x (y:ys)
    |x == y = 1 + numberOfOccurences x ys
	|otherwise = numberOfOccurences x ys