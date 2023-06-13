{-# OPTIONS_GHC -Wall #-}
module Kalyta07 where
import Data.List(sort) -- had to add this in order to implrment some functions



data BinTreeM a = EmptyM 
                | NodeM a Int (BinTreeM a) (BinTreeM a) deriving (Show, Eq) 

-- B-дерево порядку t (NodeB kl tl) =>  
--      t-1 <= length kl <= 2*t-1  &&  t <= length tl <= 2*t
data Btree a =  NodeB [a] [Btree a] deriving (Show, Eq)

-- головні характеристики B-дерево  (BInform heigth min max)
data BInform a = BInform {hB::Int, minB::a, maxB::a} deriving (Show, Eq)

-- Задача 1 ------------------------------------
isSearch :: (Ord a) => BinTreeM a -> Bool
isSearch EmptyM = True
isSearch (NodeM v k tl tr) = if (k <= 0) then False -- if the int =< 0 then false
   else isSearch tr && isSearch tl && check v tl tr

check :: (Ord a) => a -> BinTreeM a -> BinTreeM a -> Bool
check _ (EmptyM) (EmptyM) = True
check a (EmptyM) (NodeM b _ _ _) = a < b
check a (NodeM c _ _ _) (EmptyM) = a > c
check a (NodeM c _ _ _) (NodeM b _ _ _) = a < b && a > c


-- Задача 2 ------------------------------------
elemSearch :: (Ord a) => BinTreeM a -> a -> Bool
elemSearch EmptyM _ = False
elemSearch (NodeM n _ tl tr) v = if (n == v) then True
    else if (v < n)  then elemSearch tl v
    else elemSearch tr v


-- Задача 3 ------------------------------------
insSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a 
insSearch EmptyM x = NodeM x 1 EmptyM EmptyM
insSearch (NodeM n k tl tr) v = if (v == n) then NodeM n (k + 1) tl tr
      else if (v < n) then NodeM n k (insSearch tl v) tr
      else NodeM n k tl (insSearch tr v)



-- Задача 4 ------------------------------------
delSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a 
delSearch = undefined



-- Задача 5 ------------------------------------
sortList :: (Ord a) => [a] -> [a]
sortList l = tT (foldl insSearch EmptyM l)

tT :: BinTreeM a -> [a]
tT EmptyM = []
tT (NodeM v k tl tr) = tT tl ++ replicate k v ++ tT tr 


-- Задача 6 ------------------------------------
findBInform :: (Bounded a, Ord a) => Btree a ->  BInform a
findBInform (NodeB [] _)  = error "ERROR: List Of Keys is EMPTY"

findBInform (NodeB tr []) = BInform 0 (head tr) (last tr)

findBInform tr = BInform (h tr) (ll tr) (rr tr) 

ll :: Btree a -> a
ll (NodeB n []) = head n
ll (NodeB _ tr) = ll (head tr)

rr :: Btree a -> a
rr (NodeB k []) = last k
rr (NodeB _ tr) = rr (last tr)

h :: Btree a -> Int
h (NodeB _ []) = 0
h (NodeB _ tr) = (h (head tr)) + 1

-- Задача 7 ------------------------------------
isBtree  :: (Bounded a, Ord a) => Int -> Btree a -> Bool 
isBtree = undefined

-- Задача 8 ------------------------------------
eqBtree :: (Bounded a, Ord a) => Int -> Btree a -> Btree a -> Bool 
eqBtree _ tr1 tr2 = if (sort (tTS  tr1) == sort (tTS  tr2)) then True
    else False

tTS :: Btree a -> [a]
tTS (NodeB keys str) = keys ++ (concatMap tTS str)

-- Задача 9 ------------------------------------
elemBtree :: Ord a => Btree a -> a -> Bool
elemBtree tr v = elem v (tTS tr)

-- Задача 10 ------------------------------------
insBtree :: Ord a => Int -> Btree a -> a -> Btree a
insBtree = undefined


isFull :: Ord a => Int -> Btree a -> Bool
isFull = undefined

insertKey :: Ord a => a -> [a] -> [a]
insertKey = undefined

decomposeNodeB :: Ord a => a -> [a] -> [Btree a] -> 
                        ([a], [a], [Btree a], Btree a, [Btree a])
decomposeNodeB = undefined

splitAtB :: Ord a => Int -> Btree a -> (Btree a, a, Btree a)
splitAtB = undefined


---------------------Тестові дані - Дерева пошуку -------
bm :: BinTreeM Char
bm = NodeM  't' 2  
            (NodeM 'a' 1  EmptyM 
                    (NodeM 'e' 1 
                             (NodeM 'd' 2 EmptyM EmptyM)
                             (NodeM 'f' 1 EmptyM EmptyM)
                    )
            ) 
            (NodeM 'w' 2  EmptyM EmptyM)   

tBt1 :: Btree Char 
tBt1 = NodeB "L"
       [ NodeB "DG" 
          [ NodeB "AC" [], NodeB "EE" [], NodeB "HK" []
          ]
       , NodeB "PU" 
          [ NodeB "MM" [], NodeB "RS" [], NodeB "UW" []
          ]
       ]

tBt2 :: Btree Char 
tBt2 = NodeB "GP"
       [ NodeB "ACDEE" [], NodeB "HKLMM" [], NodeB "RSUUW" []
       ]

tBt5 :: Btree Char 
tBt5 = NodeB "GMPX"
       [ NodeB "ACDE" [] , NodeB "JK" [], NodeB "NO" []
       , NodeB "RSTUV" [], NodeB "YZ" []
       ]

tBt6 :: Btree Char 
tBt6 = NodeB "GMPX"
       [ NodeB "ABCDE" [], NodeB "JK" [], NodeB "NO" []
       , NodeB "RSTUV" [], NodeB "YZ" []
       ]

tBt7 :: Btree Char 
tBt7 = NodeB "GMPTX"
       [ NodeB "ABCDE" [], NodeB "JK" [], NodeB "NO" []
       , NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
       ]

tBt8 :: Btree Char 
tBt8 = NodeB "P"
       [ NodeB "GM"
          [ NodeB "ABCDE" [], NodeB "JKL" [], NodeB "NO" []
          ]
       , NodeB "TX" 
          [ NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
          ]
       ]

tBt9 :: Btree Char 
tBt9 = NodeB "P"
       [ NodeB "CGM"
          [ NodeB "AB" [], NodeB "DEF" []
          , NodeB "JKL" [], NodeB "NO" []
          ]
       , NodeB "TX" 
          [ NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
          ]
       ]
