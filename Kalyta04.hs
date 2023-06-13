{-# OPTIONS_GHC -Wall #-}
module Kalyta04 where
import Data.List(nub, sort)


type GraphS = (Int,[(Int,Int)])            
type Graph  = [[Int]]

-- Задача 1 ------------------------------------
isOrdinary :: Graph -> Bool 
isOrdinary gr = isLoop gr && isMultigraph gr && twoWayApexesPresent gr


isLoop :: Graph -> Bool --checks for loops
isLoop l = null [i | (i, x) <- zip [0..] l, elem i x]

isMultigraph :: Graph -> Bool --checks if given graph is a multigraph
isMultigraph [] = True
isMultigraph (d:mg) = isMultigraph mg && noDuplicates d

noDuplicates :: (Ord a) => [a] -> Bool --finds duplicates
noDuplicates d = length (nub d) == length d


twoWayApexesPresent :: Graph -> Bool --checks if two apexes are connected two ways
twoWayApexesPresent tw = null [i | (i, a) <- zip [0..] tw, b <- a, notElem i (tw !! b)]



-- Задача 2 ------------------------------------
fromGraph :: Graph -> GraphS 
fromGraph gr = (length gr - 1, [(i, b) | (i, a) <- zip [0..] gr, b <- a])


-- Задача 3 ------------------------------------
toGraph :: GraphS -> Graph 
toGraph grS = [[y | (x, y) <- snd grS, x == a] | a <- [0..(fst grS)]]



-- Задача 4 ------------------------------------
shortWay :: Graph -> Int -> Int -> [Int]
shortWay a b = shortest . ways a b 


shortest :: [[Int]] -> [Int] -- find the shortest list from the given list of lists
shortest [] = []
shortest z = snd $ minimum $ [(length a, a) | a <- z]

ways :: Graph -> Int -> Int -> [[Int]] -- find all routes from a to b in given graph
ways gr a b = [reverse (x:y)| k <- (allWays gr a), (x:y) <- k, x == b]


allWays :: Graph -> Int -> [[[Int]]]
allWays gr a = until conditWs stepWs [[[a]]]
    where conditWs :: [[[Int]]] -> Bool
          conditWs = null . head
          stepWs :: [[[Int]]] -> [[[Int]]]
          stepWs x = [t:(z:y) | (z:y) <- head x, notElem z y, t <- gr !! z] : x



-- Задача 5 ------------------------------------
isConnecting :: Graph -> Bool 
isConnecting gr = length (moveN gr 0) == length gr


moveN :: Graph -> Int -> [Int]
moveN gr x = snd $ until conditN (step gr) ([x],[])

conditN :: ([Int],[Int]) -> Bool
conditN (cN, _) = null cN

step :: Graph -> ([Int],[Int]) -> ([Int],[Int])
step gr (cN, o) =
      let   prev = cN ++ o
            cNN = cN >>= (gr!!)
            cNNN = filter (`notElem` prev) cNN
            new = nub cNNN
      in    (new, prev)



-- Задача 6 ------------------------------------
components :: Graph -> [[Int]]
components gr = nub [sort $ moveN gr a | a <- apexes gr]


-- Задача 7 ------------------------------------
eccentricity :: Graph -> Int -> Int
eccentricity gr v = maximum [length $ shortWay gr v a | a <- apexes gr] - 1


-- Задача 8 ------------------------------------
findDiameter :: Graph -> Int
findDiameter = maximum . allEccentricities

findRadius :: Graph -> Int
findRadius = minimum . allEccentricities


allEccentricities :: Graph -> [Int]
allEccentricities gr = [eccentricity gr a | a <- apexes gr]

-- Задача 9 ------------------------------------
findCenter :: Graph -> [Int] 
findCenter gr = let rad = findRadius gr
    in [i | (o,i) <- eccWApexes gr, o == rad]

eccWApexes :: Graph -> [(Int, Int)]
eccWApexes gr = [(eccentricity gr a, a) | a <- apexes gr]

-- Задача 10 ------------------------------------
shortWayAll :: Graph -> Int -> Int -> [[Int]] 
shortWayAll gr a b =
    let ws = ways gr a b
        lDist = leastDistance ws
    in filter (\z -> length z == lDist) ws

--returns length of the shortest list in a list of lists
leastDistance :: [[Int]] -> Int
leastDistance [] = 0
leastDistance y = fst $ minimum $ [(length x, x) | x <- y]



--- universal helper functions ---

apexes :: Graph -> [Int] --list of all apexes of given Graph
apexes ap = [0..(length ap - 1)]

---------------------Тестові дані - Графи -------
gr1S, gr2S:: GraphS
gr1S = (5,[(0,1),(0,2),(0,3),(1,0),(1,3),(1,4),
           (2,0),(2,4),(2,5),(3,0),(3,1),(4,1),(4,2),(5,2)])
gr2S = (7,[(0,1),(0,3),(1,0),(1,2),(2,1),(2,3),(3,0),(3,2),
           (4,5),(4,6),(5,4),(5,6), (6,4),(6,5)])

gr1, gr2:: Graph
gr1 = [[1,2,3],[0,3,4],[0,4,5],[0,1],[1,2],[2]]
gr2 = [[1,3],[0,2],[1,3],[0,2],[5,6],[4,6],[4,5],[]]
