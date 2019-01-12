module Main where

main :: IO ()
main = putStrLn (show(quicksort([4,3,2,1])))


quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:[]) = [x]
quicksort (x:xs) = 
        lessthan++equals++greaterthan
        where
            lessthan = quicksort [ x | x <- (x:xs), x < pivot]
            greaterthan = quicksort [ x | x <- (x:xs), x > pivot]
            equals = [ x | x <- (x:xs), x == pivot]
            pivot = pickapivot(x:xs)

quicksort' ::  [Int] -> [Int]
quicksort' (x:[]) = [x]
quicksort' lon@(x:xs) = lessthan++[x]++greaterthan
    where
        lessthan = quicksort [ y | y <- lon, y < x]
        greaterthan = quicksort [ y | y <- lon, y > x]


pickapivot :: [Int] -> Int
pickapivot (x:xs) = last xs



