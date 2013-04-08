-- Problem 11: Modified run-length encoding of a list.
-- If an element has no duplicates it is simply copied.
-- > encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']

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

-- Problem 12: Decode the above encoding.
-- > decodeModified
--     [Multiple 4 'a',Single 'b',Multiple 2 'c',
--      Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"

decodeModified :: [RLEElement a] -> [a]
decodeModified xs = decodeModified_ xs []

decodeModified_ :: [RLEElement a] -> [a] -> [a]
decodeModified_ [] acc = acc
decodeModified_ (x:xs) acc = decodeModified_ xs $ acc ++ decodeOne x
  where decodeOne (Multiple n el) = replicate n el
        decodeOne (Single el) = [el]
