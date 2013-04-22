split :: [a] -> Int -> ([a], [a])
split []     _ = ([], [])
split xs     0 = ([], xs)
split (x:xs) n = (x:(fst others), (snd others))
                 where others = split xs (n-1)

-- tail recursive version
split2 :: [a] -> Int -> ([a], [a])
split2 = split2_ []

split2_ :: [a] -> [a] -> Int -> ([a], [a])
split2_ acc []     _ = (acc, [])
split2_ acc xs     0 = (acc, xs)
split2_ acc (x:xs) n = split2_ (acc++[x]) xs (n-1)
