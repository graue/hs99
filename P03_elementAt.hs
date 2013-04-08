-- Problem 3:
-- Find the K'th element of a list, considering the first element number 1.
-- Example:
--
-- Prelude> elementAt [1,2,3] 2
-- 2
-- Prelude> elementAt "haskell" 5
-- 'e'

elementAt :: [a] -> Int -> a
elementAt list n = list !! (n-1)

elementAt' :: [a] -> Int -> a
elementAt' (x:_) 1 = x
elementAt' (_:xs) n = elementAt' xs (n-1)

-- elementAt' "haskell" 5
-- = elementAt' "askell" 4
-- = elementAt' "skell" 3
-- = elementAt' "kell" 2
-- = elementAt' "ell" 1
-- = "e"
