range :: Int -> Int -> [Int]
range first last
    | first <= last = (first:(range (first+1) last))
    | otherwise     = []
