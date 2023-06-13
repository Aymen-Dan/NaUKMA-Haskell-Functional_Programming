{-# OPTIONS_GHC -Wall #-}
module Kalyta01 where

-- Задача 1 -----------------------------------------
power3 :: [Integer]
power3 = [x*x*x | x <- [1..]]

-- Задача 2 -----------------------------------------
toPower3 :: [Integer]
toPower3 = [3^n | n <- [1..]]

-- Задача 3 -----------------------------------------
-- так і не вишло приьрати попередження :(
sumPower3 :: Integer -> Integer
sumPower3 n = sum ([3^x | x <- [1..n]])

-- Задача 4 -----------------------------------------
sumPower :: Integer -> Integer -> Integer
sumPower m n = sum ([m^i | i <- [1..n]])



-- Задача 5 -----------------------------------------
lessMe :: [Int] -> [Int]
lessMe xs = if null xs then []
else map (lessThan xs) xs 

-- find how many elements in list are > x
lessThan :: [Int] -> Int -> Int
lessThan xs x = length [i | i <- xs, i < x ]


-- Задача 6 -----------------------------------------
hailstone :: Int -> Int
hailstone n =  if even n then div n 2
else n*3 + 1

-- Задача 7 -----------------------------------------
hailSeq :: Int -> [Int]
hailSeq n = if n == 1 then [1]
else n : hailSeq (hailstone n)

-- Задача 8 -----------------------------------------
allHailSeq :: [[Int]]
allHailSeq = [hailSeq x | x <- [1..]]

-- Задача 9 -----------------------------------------
firstHailSeq :: Int -> Int
firstHailSeq l = head (head [xs | xs <- allHailSeq, length xs == l])

