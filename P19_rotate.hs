rotate :: [a] -> Int -> [a]
rotate xs n
    | n < 0         = rotate xs (n + length xs)
    | n > length xs = rotate xs (n - length xs)
    | otherwise     = (drop n xs) ++ (take n xs)
