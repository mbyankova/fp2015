removeAt :: Int -> [a] -> [a]
removeAt index xs 
     |index < 0 || index >= (length xs) = error "Index out of bounds"
     |otherwise = (take index xs) ++ (drop (index + 1) xs)
    