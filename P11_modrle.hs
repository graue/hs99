-- Problem 11: Modified run-length encoding of a list.
-- If an element has no duplicates it is simply copied.
-- > encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']

module P11_modrle (RLEElement(..), encodeModified)
where

data RLEElement a = Multiple Int a | Single a
    deriving(Show)

encodeModified :: (Eq a) => [a] -> [RLEElement a]
encodeModified [] = []
encodeModified (x:xs) = let sames = takeWhile (\y -> x == y) (x:xs)
                            numSames = length sames
                            encodeSames = if numSames == 1
                                            then Single x
                                            else Multiple numSames x
                        in encodeSames :
                            (encodeModified $ drop numSames (x:xs))
