-- Problem 2:
-- Find the last but one element of a list.
--
-- Prelude> myButLast [1,2,3,4]
-- 3
-- Prelude> myButLast ['a'..'z']
-- 'y'

myButLast :: [a] -> a
-- myButLast (x:x':[]) = x
-- myButLast (x:xs) = myButLast xs

myButLast [x,_] = x
myButLast (_:xs) = myButLast xs
