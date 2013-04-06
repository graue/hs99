-- Problem 10: Run-length encoding of a list.
-- > encode "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode (x:xs) = let sames = takeWhile (\y -> x == y) (x:xs)
                    numSames = length sames
                in (numSames, x) : (encode (drop numSames (x:xs)))
