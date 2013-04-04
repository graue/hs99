-- Problem 5: Reverse a list
-- Example:
-- Prelude> myReverse "A man, a plan, a canal, panama!"
-- "!amanap ,lanac a ,nalp a ,nam A"

myReverse :: [a] -> [a]
myReverse xs = myReverse_ xs []
  where myReverse_ [] acc = acc
        myReverse_ (x:xs) acc = myReverse_ xs (x:acc)

-- Above version reverses a 10 million element list in 6.2 seconds.
-- The below, naive version took more than 20 seconds without completing.

myReverseSlow :: [a] -> [a]
myReverseSlow [] = []
myReverseSlow (x:xs) = myReverseSlow xs ++ [x]
