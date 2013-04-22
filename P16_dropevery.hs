dropEvery :: [a] -> Int -> [a]
dropEvery xs n = dropEvery_ (n-1) xs n

dropEvery_ :: Int -> [a] -> Int -> [a]
dropEvery_ _     []     _ = []
dropEvery_ 0     (_:xs) n = dropEvery_ (n-1) xs n
dropEvery_ phase (x:xs) n = (x:(dropEvery_ (phase-1) xs n))
