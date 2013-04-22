slice :: [a] -> Int -> Int -> [a]
slice []     _  _  = []
slice (x:xs) lo hi
    | lo > hi   = []
    | lo >  1   = slice xs (lo-1) (hi-1)
    | hi <= 1   = [x]
    | otherwise = (x:(slice xs 1 (hi-1)))
