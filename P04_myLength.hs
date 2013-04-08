-- Problem 4, find number of elements of a list.

myLength :: [a] -> Int
myLength xs = myLength_ xs 0
  where myLength_ [] n = n
        myLength_ (_:xs) n = myLength_ xs (n+1)
