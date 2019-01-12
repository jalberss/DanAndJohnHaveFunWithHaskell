module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"


Quicksort :: [Int] -> [Int]
Quicksort x:[] = [x]
Quicksort (x:xs) = 
    let pivot = Pickapivot(x:xs) in 
        lessthan++equals++greaterthan
        where
            lessthan = Quicksort [ x | x <- (x:xs), x < pivot]
            greaterthan = Quicksort [ x | x <- (x:xs), x > pivot]
            equals = [ x | x <- (x:xs), x == pivot]

Pickapivot :: [Int] -> Int
Pickapivot (x:xs) = last xs



