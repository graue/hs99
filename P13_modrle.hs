-- Problem 11: Direct run-length encoding of a list.
-- If an element has no duplicates it is simply copied.
-- > encodeDirect "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']

module P13_modrle (RLEElement(..), encodeDirect)
where

data RLEElement a = Multiple Int a | Single a
    deriving(Show)

countWhile :: (a -> Bool) -> [a] -> Int
countWhile f [] = 0
countWhile f (x:xs)
    | f x       = 1 + countWhile f xs
    | otherwise = 0

encodeDirect :: (Eq a) => [a] -> [RLEElement a]
encodeDirect [] = []
encodeDirect (x:xs) = let numSames = countWhile (\y -> x == y) (x:xs)
                          encodeSames = if numSames == 1
                                            then Single x
                                            else Multiple numSames x
                        in encodeSames :
                            (encodeDirect $ drop numSames (x:xs))
