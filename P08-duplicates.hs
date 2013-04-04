-- Problem 8: Eliminate consecutive duplicates of list elements.

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:x':xs) = if x == x'
                      then (compress (x:xs))
                      else x:(compress (x':xs))

