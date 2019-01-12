module Main where

import Control.Parallel
import Control.Parallel.Strategies
import Control.Exception
import Data.Time.Clock
import Text.Printf
import System.Environment




main :: IO ()
main = putStrLn (show(quicksort'(reverse([1..9999]))))


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

-- quicksort' ::  [Int] -> [Int]
-- quicksort' (x:[]) = [x]
-- quicksort' lon@(x:xs) = lessthan++[x]++greaterthan
--     where
--         lessthan = quicksort [ y | y <- lon, y < x]
--         greaterthan = quicksort [ y | y <- lon, y > x]

quicksort' :: [Int] -> [Int]
quicksort' [] = []
quicksort' (x:[]) = [x]
quicksort' lon@(x:xs) =
  lessthan++[x]++greaterthan
  where
    lessthan = quicksort' $ runEval $ parFilter (<x) xs    
    greaterthan = quicksort' $ runEval $ parFilter (>x) xs
  


parFilter :: (a -> Bool) -> [a] -> Eval [a]
parFilter f [] = return []
parFilter f (x:xs) = do
  b <- rpar (filter f [x]) 
  bs <- parFilter f xs
  return (b++bs)


pickapivot :: [Int] -> Int
pickapivot (x:xs) = last xs



--- Rpar.hs -- 

-- <<fib
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
-- >>

-- <<main
-- main = do
--   [n] <- getArgs
--   let test = [test1,test2,test3,test4] !! (read n - 1)
--   t0 <- getCurrentTime
--   r <- evaluate (runEval test)
--   printTimeSince t0
--   print r
--   printTimeSince t0
-- >>

-- <<test1
test1 = do
  x <- rpar (fib 50)
  y <- rpar (fib 51)
  return (x,y)
-- >>

-- <<test2
test2 = do
  x <- rpar (fib 36)
  y <- rseq (fib 35)
  return (x,y)
-- >>

-- <<test3
test3 = do
  x <- rpar (fib 36)
  y <- rseq (fib 35)
  rseq x
  return (x,y)
-- >>

-- <<test4
test4 = do
  x <- rpar (fib 36)
  y <- rpar (fib 35)
  rseq x
  rseq y
  return (x,y)
-- >>

printTimeSince t0 = do
  t1 <- getCurrentTime
  printf "time: %.2fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)
