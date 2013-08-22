-- Sorting a list of lists according to length of sublists

-- a) We suppose that a list contains elements that are lists themselves. The
-- objective is to sort the elements of this list according to their length.
-- E.g. short lists first, longer lists later, or vice versa.
--
--  Prelude>lsort ["abc","de","fgh","de","ijkl","mn","o"]
--  Prelude>["o","de","de","mn","abc","fgh","ijkl"]

mergeSortBy :: Ord b => (a -> b) -> [a] -> [a]
mergeSortBy _ []  = []
mergeSortBy _ [x] = [x]
mergeSortBy f xs  = combineSortedListsBy f
                      (mergeSortBy f firstHalf)
                      (mergeSortBy f secondHalf)
                    where (firstHalf, secondHalf) = splitInHalf xs

splitInHalf :: [a] -> ([a], [a])
splitInHalf xs = splitAt halfLength xs
                 where halfLength = (length xs) `div` 2

combineSortedListsBy :: Ord b => (a -> b) -> [a] -> [a] -> [a]
combineSortedListsBy f = loop []
    where loop acc []     ys     = (reverse acc)++ys
          loop acc xs     []     = (reverse acc)++xs
          loop acc (x:xs) (y:ys)
              | f y <  f x = loop (y:acc) (x:xs) ys
              | otherwise  = loop (x:acc) xs (y:ys)

lsort :: [[a]] -> [[a]]
lsort = mergeSortBy length
