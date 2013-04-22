removeAt :: Int -> [a] -> [a]
removeAt n (x:xs)
    | n <= 1    = xs
    | otherwise = (x:(removeAt (n-1) xs))
removeAt _ [] = []
