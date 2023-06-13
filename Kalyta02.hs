{-# OPTIONS_GHC -Wall #-}
module Kalyta02 where

-- Задача 1 -----------------------------------------
sumFl :: [Integer] -> Integer
sumFl xs = foldl (+) 0 xs
  
-- Задача 2 ----------------------------------------- 
productFr :: [Integer] -> Integer
productFr xs = foldr (*) 1 xs

-- Задача 3 -----------------------------------------
concatFr :: [Int] -> [Int] -> [Int]
concatFr xs ys = foldr (:) ys xs

-- Задача 4 -----------------------------------------
sortInsert :: [Int] -> [Int]
insert :: [Int] -> Int -> [Int]

insert [] v = [v]
insert (x:xs) v = if x < v then x : insert xs v
else v : insert xs x

sortInsert xs = foldl insert [] xs

-- Задача 5 -----------------------------------------
findIndices ::(Int -> Bool) -> [Int] -> [Int] 
findIndices = undefined

-- Задача 6 -----------------------------------------
allReverse :: [String] -> [String]
allReverse xxs = reverse (map reverse xxs)

-- Задача 7  -----------------------------------------
noDigits :: String -> String
noDigits = undefined

-- Задача 8 ------------------------------------------
cntGood :: [Int -> Bool] -> Int -> Int
cntGood ps v = length [x | x <- ps, x v]

-- Задача 9 ------------------------------------------
trianglePas :: [[Integer]]
trianglePas = iterate (\x -> zipWith (+) ([0] ++ x) (x ++ [0])) [1]

-- Задача 10 -----------------------------------------
factorialsM :: [Integer]
factorialWithZero :: [Integer]
factorialWithZero = 1 : zipWith (*) factorialWithZero [1..]
factorialsM = (tail factorialWithZero)