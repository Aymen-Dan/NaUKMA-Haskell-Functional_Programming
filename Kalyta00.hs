 {-# OPTIONS_GHC -Wall #-}
module Kalyta00 where

import Data.List ( (\\) )

data Quaternion = Quaternion Double Double Double Double 
                  deriving (Eq)

type Graph  = [[Int]]

data Tree23 a  = Leaf a   
               | Fork2 (Tree23 a) a (Tree23 a) 
               | Fork3 (Tree23 a) a (Tree23 a) a (Tree23 a)
               | Null23     -- порожнє 2-3-дерево!!!
               deriving (Eq, Show)

-- Задача  1 -----------------------------------------
instance Show Quaternion where
   show (Quaternion l i j k) = 
    show l ++ (if i >= 0 then " + " ++ show i ++ "i "
               else show i ++ "i ") ++ (if j >= 0 then "+ " ++ show j ++ "j "
                                                 else show j ++ "j ") ++ (if k >= 0 then "+ " ++ show k ++ "k"
                                                                         else show k ++ "k") 


-- Задача 2 -----------------------------------------
plusQuaternion :: Quaternion -> Quaternion -> Quaternion
plusQuaternion (Quaternion l1 i1 j1 k1) (Quaternion l2 i2 j2 k2) = Quaternion (l1+l2) (i1+i2) (j1+j2) (k1+k2)


-- Задача 3 -----------------------------------------
timesQuaternion :: Quaternion -> Quaternion -> Quaternion
timesQuaternion (Quaternion l1 i1 j1 k1) (Quaternion l2 i2 j2 k2) = Quaternion ((l1*l2) - (i1*i2) - (j1*j2) - (k1*k2)) ((l1*i2) + (l2*i1) + (j1*k2) - (j2*k1)) ((l1*j2) + (l2*j1) - (i1*k2) + (k1*i2)) ((l1*k2) + (l2*k1) + (i1*j2) - (i2*j1))


--- Задача 4 ----------------------------------------
instance Num Quaternion  where
    (+)   = plusQuaternion
    (*)   = timesQuaternion
    negate (Quaternion l i j k) = (Quaternion (negate l) (negate i) (negate j) (negate k))
    fromInteger q = (Quaternion (fromInteger q) 0 0 0)
    abs q        = (Quaternion (mySqrt q) 0 0 0)
    signum q@(Quaternion l i j k) = let qq = mySqrt q
                                       in (Quaternion (l/qq) (i/qq) (j/qq) (k/qq))


mySqrt :: Quaternion -> Double
mySqrt (Quaternion l i j k) = sqrt((l*l) + (i*i) + (j*j) + (k*k))



-- Задача 5 -----------------------------------------
isTournament :: Graph -> Bool
isTournament gr = let edges g = [(x, y)| x <- [0..(length g - 1)], y <- g !! x] 
                      hlpr [] = 0
                      hlpr (g : gr) = length (g : gr) + length gr +1
                  in if length (edges gr) == hlpr gr then True
                     else False
    
-- Задача 6 -----------------------------------------
gamiltonWay :: Graph -> Maybe [Int]
gamiltonWay gr | isGr gr = if (length gr) + 1 == length(gamiltonWayHLPR gr) then Just (reverse (gamiltonWayHLPR gr))
                              else Nothing
               | otherwise = error "ERROR: graph is not oriented"

                  where isGr gr = foldl1 (&&) [(setChk x) && (hasOnly x (nds gr)) | x <- gr] 
                                     where setChk [] = True
                                           setChk (s : ss) = if elem s ss then False
                                                            else setChk ss
--basically a nodes function
nds :: Graph -> [Int]
nds gr = [0, 1..(length gr - 1)]


hasOnly :: [Int] -> [Int] -> Bool
hasOnly [] _ = True
hasOnly _ [] = False
hasOnly xx yy = foldl1 (&&) [elem x yy | x <- xx]


gamiltonWayHLPR :: Graph -> [Int]
gamiltonWayHLPR gr = theLongest(toListWithEnd 0 (simpCcls gr 0))
                     where theLongest [] = []
                           theLongest (x : []) = x
                           theLongest (x : [y]) = if (length x > length y) then x
                                                  else y
                           theLongest (x : xx) = if (length x > length (head xx)) then theLongest (x: (tail xx))
                                                 else theLongest xx


                           toListWithEnd m www = [x | ww <- www, x <- ww, head x == m]

simpCcls :: Graph -> Int -> [[[Int]]] 
simpCcls gr v = until yesCond (sT gr) [[[v]]]
                       where yesCond sss = null (head sss)
                             sT _ [] = error "ERROR: Empty list"
                             sT gr zzz@(wzz : _) = [a : z | z@(x : xx) <- wzz, a <- gr !! x, notElem a (xx \\ [0])] : zzz
                             

-- Задача 7 -----------------------------------------
isAcyclic :: Graph -> Bool 
isAcyclic gr = if (findFunc(cclCheck gr) (aW gr) == Nothing) then True -- check with Cycle check
               else False


--finder function
findFunc :: (a -> Bool) -> [a] -> Maybe a
findFunc _ [] = Nothing
findFunc ff (x : xx) = if ff x then Just x
                    else findFunc ff xx

--checks if Cycle
cclCheck :: Graph -> [Int] -> Bool
cclCheck cgr v = elem (last v) (cgr !! (head v))


--all the working ways
aW :: Graph -> [[Int]]
aW gr = concat [concat (until nullCheck (stp gr) [[[v]]]) | v <- nds gr]
             where nullCheck xx = null (head xx) --check if Null

                   stp _ [] = [] 
                   stp grr ww@(wy : _) = [x : w| w@(t : tt) <- wy, notElem t tt, x <- (grr !! t)] : ww --take a step


-- Задача 8 -----------------------------------------
isTopolSort :: Graph -> [Int] -> Bool 
isTopolSort _ [] = True
isTopolSort gr (t : ts) = (isTopolSort gr ts) && (all (\t -> elem t ts) (gr !! t)) --a little weird but works


-- Задача 9 -----------------------------------------
longWay :: Graph -> Int -> Int -> Maybe [Int]
longWay gr a b = let array = ddd gr a b []
                           where ddd grr a1 b1 p = if (a1 == b1) then [[a1]]
                                                else if (elem a1 p) then []
                                                else foldl (\x z -> map (\u -> a1 : u) (ddd grr z b1 (a : p)) ++ x) [] (grr !! (a1))

                 in if (null array) then Nothing -- returns nothing if no way
                    else Just (snd ( maximum ( map (\y -> (length y, y)) (array)))) -- searches for way


--- Задача 10 ----------------------------------------
merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge xx [] = xx
merge [] yy = yy
merge (x : xx) (y : yy) = if (x == y) then mySorter (delCpys (y : merge xx yy)) --when elem eq
           else if (x < y) then mySorter (delCpys (x : y : merge xx yy)) --when elem1 < elem2
           else mySorter (delCpys (y : x : merge xx yy)) -- when elem1 > elem2

                where delCpys xs = delCpysHLPR xs [] -- delete repeating values

                                   where delCpysHLPR [] _ = [] 
                                         delCpysHLPR (s : ss) found = if (elem s found) then delCpysHLPR ss found -- hlpr func
                                                                      else s : delCpysHLPR ss (s : found)

mySorter :: Ord a => [a] -> [a] --sorting
mySorter [] = []
mySorter (x : xx) = let less  = filter (< x) xx
                        more = filter (>= x) xx
                    in (mySorter less) ++ [x] ++ (mySorter more)


--- Задача 11 ----------------------------------------
intToString :: Int -> Int -> String
intToString n m = intToStringHLPR n m ""


intToStringHLPR:: Int -> Int -> String -> String
intToStringHLPR 0 _ ans = show ans
intToStringHLPR z m ans = let toLtr x = case x of --a bit of help for translations
                                    10 -> "a"
                                    11 -> "b"
                                    12 -> "c"
                                    13 -> "d"
                                    14 -> "e"
                                    15 -> "f"
                                    _ -> show x

                          in if (z >= m) then intToStringHLPR (div z m) m ((toLtr (mod z m)) ++ ans) -- the actual meat of the func
                             else (toLtr z) ++ ans


--- Задача 12 ----------------------------------------
stringToInt :: Int -> String -> Maybe Int
stringToInt n xs = stringToIntHLPR n xs (length xs -1) 0

stringToIntHLPR :: Int -> String -> Int -> Int -> Maybe Int
stringToIntHLPR _ [] _ str = Just str
stringToIntHLPR m (x : xx) l ans = if (chrToInt x >= m) then Nothing
                                   else stringToIntHLPR m xx (l - 1) (((chrToInt x) * m^l) + ans)
                                               where chrToInt x = case x of
                                                             'a' -> 10
                                                             'b' -> 11
                                                             'c' -> 12
                                                             'd' -> 13
                                                             'e' -> 14
                                                             'f' -> 15
                                                             _ -> read [x]



--- Задача 13 ----------------------------------------
genExpr :: Int -> Int -> [String]
genExpr a b = [fst e | e <- genExprHLPR (reverse (toLst a)), snd e == b]
               where toLst 0 = []
                     toLst z = toLst (div z 10) ++ [mod z 10]

genExprHLPR :: [Int] -> [(String, Int)]
genExprHLPR [] = [([], 0)]
genExprHLPR [a] = [(show a, a)]
genExprHLPR (a : aa) = [( fst(b) ++ ('+' : show a), (snd b) + a ) | b <- genExprHLPR aa] ++
                   [( fst(b) ++ ('-' : show a), (snd b) - a ) | b <- genExprHLPR aa] ++ [( fst(b) ++ ('*' : show a), (snd b) * a ) | b <- genExprHLPR aa]
                   

--- Задача 14 ----------------------------------------
genExprBracket :: Int -> Int -> [String]
genExprBracket = undefined

--- Задача 15 ----------------------------------------
isTree23  :: (Ord a) => Tree23 a -> Bool 
isTree23 Null23 = True
isTree23 (Leaf a) = True
isTree23 (Fork2 tl a tr) = (a == minTree tr) && (a >= maxTree tl)
isTree23 (Fork3 tl a tm b tr) = (a == minTree tm) && (b >= maxTree tm) && (b == minTree tr) && (a >= maxTree tl)
                                           
minTree :: Tree23 p -> p
--minTree Null23 isn't neccesary
minTree (Leaf p) = p
minTree (Fork2 Null23 p _) = p
minTree (Fork3 Null23 p _ _ _) = p
minTree (Fork2 tl _ _) = minTree tl
minTree (Fork3 tl _ _ _ _) = minTree tl

maxTree :: Tree23 p -> p
--maxTree Null23 isn't neccesary
maxTree (Leaf p) = p
maxTree (Fork2 _ p Null23) = p
maxTree (Fork3 _ _ _ p Null23) = p
maxTree (Fork2 _ _ tr) = maxTree tr
maxTree (Fork3 _ _ _ _ tr) = maxTree tr

--- Задача 16 ----------------------------------------
eqTree23 :: (Ord a) => Tree23 a -> Tree23 a -> Bool
--changed tr1 & tr2 to ttr1 & ttr2 to avoid redefinition
eqTree23 ttr1 ttr2 = let toList Null23 = []
                         toList (Leaf a) = [a]
                         toList (Fork2 tl _ tr) = toList tl ++ toList tr
                         toList (Fork3 tl _ tm _ tr) = toList tl ++ toList tm ++ toList tr

                     in if (toList ttr1 == toList ttr2) then True
                        else False




--- Задача 17 ----------------------------------------
elemTree23 :: (Ord a) => Tree23 a -> a -> Bool
elemTree23 (Fork3 (x) x1 (y) y1 (z)) v = if ((v == x1) || (v == y1)) then True
                                       else if (v < x1) then elemTree23 (x) v
                                       else if ((v > x1) && (v < y1)) then elemTree23 (y) v
                                       else if (v > y1) then elemTree23 (z) v
                                       else False


elemTree23 (Fork2 (x) y (n)) v = if (v == y) then True
                                 else if (v < y) then elemTree23 (x) v
                                 else if (v > y) then elemTree23 (n) v
                                 else False
elemTree23 (Leaf l) v = l == v
elemTree23 (Null23) _ = False


--- Задача 18 ----------------------------------------
insTree23 :: (Ord a) => Tree23 a -> a -> Tree23 a
insTree = undefined

-- isTerminal tr = True <=> якщо сини вузла tr - листки !!
isTerminal :: (Ord a) => Tree23 a -> Bool
isTerminal (Fork2 (Leaf _) _ _)     = True 
isTerminal (Fork3 (Leaf _) _ _ _ _) = True
isTerminal _                        = False

-- Результат вставки вузла в 2-3-дерево, 
--   корінь якого - вузол вида Fork2 або Fork3 є об`єкт із (Tree23 a, Maybe (a, Tree23 a))
--   : (a, Nothing) - результат вставки - одне 2-3-дерево a 
--   : (a, Just (w, b)) - результат вставки два 2-3-дерева a i b (w - найменше значення в b)
--  insert v tr - додає значення v в довільне дерево tr
insert :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insert v tr | isTerminal tr = insTerm v tr
            | otherwise     = insFork v tr

-- insTerm v tr - додається значення v в дерево tr з конем - термінальний вузол 
insTerm :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insTerm = undefined

-- insFork v tr - додає значення v в дерево tr з корнем - нетермінальний вузол 
insFork :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insFork = undefined

---------------------Тестові дані 

---------------------- Графи -------
gr1, gr2, gr3:: Graph
gr1 = [[1,2,3],[2,3],[3,4],[4],[]]
gr2 = [[3,4],[0,3],[0,1,4],[2,4],[1]]
gr3 = [[1],[2],[3],[1],[0,3]]

---------------------- 2-3-дерева
tr1, tr2, tr3, tr4, tr5 :: Tree23 Int
tr1 =  Fork2 (Fork2 (Fork2 (Leaf 0) 1 (Leaf 1)) 
                     2
                    (Fork2 (Leaf 2) 3 (Leaf 3)))
              4
             (Fork2 (Fork2 (Leaf 4) 5 (Leaf 5)) 
                     6
                    (Fork2 (Leaf 6) 7 (Leaf 7)))
tr2 =  Fork3 (Fork2 (Leaf 0) 1 (Leaf 1))
              2
             (Fork3 (Leaf 2) 3 (Leaf 3) 4 (Leaf 4))
              5
             (Fork3 (Leaf 5) 6 (Leaf 6) 7 (Leaf 7))

tr3 = Fork3 (Fork2 (Leaf 2) 5 (Leaf 5))
            7
            (Fork3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Fork2 (Leaf 16) 19 (Leaf 19))

tr4 = Fork3 (Fork2 (Leaf 2) 5 (Leaf 5))
            7
            (Fork3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Fork3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))

tr5 = Fork2 (Fork2 (Fork2 (Leaf 2) 5 (Leaf 5))
                    7
                   (Fork2 (Leaf 7) 8 (Leaf 8)) 
            )
            10
            (Fork2 (Fork2 (Leaf 10) 12 (Leaf 12))
                   16
                   (Fork3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))
            )