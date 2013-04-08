-- Problem 9
-- Pack consecutive duplicates of list elements into sublists.

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = (takeWhile p (x:xs)) : (pack (dropWhile p xs))
                where p y = y == x
