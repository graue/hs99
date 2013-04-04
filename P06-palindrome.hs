-- Problem 6
-- Find out whether a list is a palindrome.
-- *Main> isPalindrome [1,2,3]
-- False
-- *Main> isPalindrome "madamimadam"
-- True
-- *Main> isPalindrome [1,2,4,8,16,8,4,2,1]
-- True

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs) = isPalindrome (init xs) && x == (last xs)

isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' xs = reverse xs == xs
