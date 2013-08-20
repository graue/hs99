insertAt :: a -> [a] -> Int -> [a]
insertAt y (x:xs) n
    | n <= 1    = (y:x:xs)
    | otherwise = (x:(insertAt y xs (n-1)))
insertAt y [] _ = [y]
