{-# OPTIONS_GHC -Wall #-}
module Kalyta05 where

import Data.List(nub,sort)

-- машина Тюрінга
data Going = No | Lt | Rt deriving (Show,Eq,Ord)
type Table = [((Int,Char),(Int,Char,Going))]
type Machine = (Int, Table)
type Config = (String,(Int,Char), String, Int)

-- опис складної машини Тюрінга 
-- L | R | P Char | G  базові машини
data Complex = Join [Complex] 
             | While Char Complex 
             | If Char Complex Complex 
             | P Char 
             | L | R | G 
             deriving (Show, Eq)

-- Задача 1.a -----------------------------------------
alphabet :: Table -> [Char]
alphabet go = orderList(getLetters go) 

getLetters :: Table -> [Char]
getLetters gL = [snd (fst x)| x <- gL] ++ [ case snd x of (_, a, _) -> a  |x <- gL] --forming an ordered list of symbols of the alphabet (that are chars from needed positions)

orderList :: [Char] -> [Char]
orderList list = sort (nub list) --ordering & cleaning



-- Задача 1.b ----------------------------------------- 
states :: Machine -> [Int]
states m =  orderIntList(stateList m)

stateList :: Machine -> [Int]
stateList stL = [fst (fst x)| x <- snd stL] --forming an ordered list of states (that are ints from needed positions)

orderIntList :: [Int] -> [Int]
orderIntList list = sort (nub list) --ordering & cleaning



-- Задача 2 -----------------------------------------
iswfMachine :: Machine -> Bool
iswfMachine m = forAllStates m && forAllSymbols m

forAllStates :: Machine -> Bool
forAllStates stts = and [x `elem` states stts| x<- [1.. fst stts]] --check if the machine is configured for all states

forAllSymbols :: Machine -> Bool
forAllSymbols symb = and [(q, x) `elem` getSteps (snd symb)|x <- alphabet (snd symb), q <- states symb] -- check if the machine is configured for all symbols of the alphabet

getSteps :: Table -> [(Int, Char)]
getSteps st = [fst x| x <- st] --get the steps needed

initCon :: Machine -> String -> Config 
-- будує початкову конфігурацію за машиною і вхідним рядком 
initCon (is,_) ""     = ("", (is,' '), "", 0)
initCon (is,_) (c:cx) = ("", (is, c), cx, 0)



-- Задача 3.a -----------------------------------------
isFinal ::  Int -> Config -> Bool
isFinal mx cnf = (case cnf of (_,_,_,a) -> a) == mx || fst (case cnf of (_,a,_,_) -> a) == 0

-- Задача 3.b -----------------------------------------
stepM :: Machine -> Config -> Config
stepM m cnf
   | getMove m cnf== Rt = moveRt m cnf
   | getMove m cnf == Lt = moveLft m cnf
   | otherwise = movent m cnf


moveRt::Machine -> Config -> Config
moveRt m x = (if getEl1 x == " " && getChr m x == ' ' then []
                   else checkStringIsEmpty (getEl1 x) ++ [getChr m x],(getStt_Config m x,
                 if null (getEl3 x) then ' '
                   else head (getEl3 x)),
                 if null (getEl3 x) then getEl3 x
                   else tail (getEl3 x), getEl4 x + 1) --moving right

moveLft::Machine->Config->Config
moveLft m x = (if null (getEl1 x) then "" else init (getEl1 x),
    (getStt_Config m x, if null (getEl1 x) then ' ' else last (getEl1 x)),
    if getEl3 x == " " && getChr m x == ' ' then [] else getChr m x : checkStringIsEmpty (getEl3 x) ,
     getEl4 x + 1) --moving left

movent::Machine->Config->Config
movent m x = (getEl1 x, (getStt_Config m x, getChr_Config m x), getEl3 x, getEl4 x + 1) --staying in place


getChr :: Machine -> Config -> Char
getChr m x = getTblEl2 (snd (filtTbl_State (snd m) (getEl2 x))) --get character from a config

filtTbl_State :: Table -> (Int, Char) -> ((Int,Char),(Int,Char,Going))
filtTbl_State tbl state = head (filter (\e -> state == fst e) tbl) --filter a table by state

getMove :: Machine->Config->Going
getMove m x = getTblEl3 (snd (filtTbl_State (snd m) (getEl2 x))) --read the move decree


getStt_Config :: Machine -> Config -> Int
getStt_Config m x = getTblEl1 (snd (filtTbl_State (snd m) (getEl2 x))) --get state 

getChr_Config :: Machine -> Config -> Char
getChr_Config m x = getTblEl2 (snd (filtTbl_State (snd m) (getEl2 x))) --get character


--some getters to make life easier--
getTblEl1 :: (Int, Char, Going) -> Int
getTblEl1 z = case z of (a,_,_) -> a

getTblEl2 :: (Int, Char, Going) -> Char
getTblEl2 z = case z of (_,a,_) -> a

getTblEl3 :: (Int, Char, Going) -> Going
getTblEl3 z = case z of (_,_,a) -> a

getEl1 :: Config -> String
getEl1 x = case x of (a,_,_,_) -> a

getEl2 :: Config -> (Int, Char)
getEl2 x = case x of (_,a,_,_) -> a

getEl3 :: Config -> String
getEl3 x = case x of (_,_,a,_) -> a

getEl4 :: Config -> Int
getEl4 x = case x of (_,_,_,a) -> a


checkStringIsEmpty :: String -> String
checkStringIsEmpty emptyS = if emptyS == " " then [] else emptyS --check if string is empty


-- Задача 4 -----------------------------------------
eval :: Machine -> Int -> String -> Maybe String
eval m mx u = evaluate m (initCon m u) mx

checIfFinConf :: (Int, Char) -> Bool
checIfFinConf (a,_) = a == 0 --check if the configuration is final

evaluate :: Machine -> Config -> Int -> Maybe String
evaluate  m x end
    | getEl4 x == end && checIfFinConf (getEl2 x) == False = Nothing
    | checIfFinConf (getEl2 x) = Just (if (getEl1 x == " " || getEl1 x ==[]) && snd (getEl2 x) == ' ' then if getEl3 x == " " then []
      else getEl3 x
      else (checkStringIsEmpty (getEl1 x) ++  (if snd (getEl2 x) == ' ' && (getEl3 x == " "||getEl3 x == []) then []
        else ([snd (getEl2 x)] ++ checkStringIsEmpty (getEl3 x)))))
    | otherwise = evaluate m (stepM m x) end


-- Задача 5.a -----------------------------------------
renum :: Int -> Machine -> Machine   
renum k m = (fst m + k, [(incST k (fst q), incTBL k (snd q))| q <- snd m])

incST::Int -> (Int,Char) -> (Int,Char)
incST k (x,y) = if x == 0 then (x,y)
else (x+k,y) --increments state

incTBL::Int -> (Int,Char,Going) -> (Int,Char,Going)
incTBL k (x,y,z) = if x == 0 then (x,y,z)
else (x+k,y,z) --increments table



-- Задача 5.b -----------------------------------------
connect :: Int -> Table -> Table
connect p t = [(fst q, newState p (snd q))|q <- t]

newState :: Int -> (Int, Char, Going) -> (Int, Char, Going)
newState p (x,y,z) = if x == 0 then (p,y,z)
else (x,y,z) --change state to a new one

  

-- Задача 6.a -----------------------------------------
seqJoin :: Machine -> Machine -> Machine 
seqJoin m1 m2 = (fst (renum (fst m2) m1), snd m2 ++ connect (fst m2) (snd (renum (fst m2) m1)) )

-- Задача 6.b -----------------------------------------
ifJoin :: Char -> String -> Machine -> Machine -> Machine
ifJoin c alf m1 m2  = (fst (renum (fst m2) m1) + 1, snd m2 ++ snd (renum (fst m2) m1) ++ [ ((fst (renum (fst m2) m1) + 1,s),
  (if s == c then fst (renum (fst m2) m1)
    else fst m2, s, No))| s <- alf ])


-- Задача 6.c -----------------------------------------
cycleJoin :: Char -> String -> Machine -> Machine 
cycleJoin c alf m = (fst m + 1, connect (fst m + 1) (snd m) ++ [ ((fst m + 1,s),
  (if s == c then fst m
    else 0, s, No))  | s<-alf ])


-- Задача 7 -----------------------------------------
build ::  String -> Complex -> Machine
build = undefined

-- Задача 8.a-----------------------------------------
subtractAbs :: Complex
subtractAbs = undefined

-- Задача 8.b-----------------------------------------
subtraction :: Complex     
subtraction = undefined

--------------------------------------------------------
--  тестові дані 
-- приклади машин Тюрінга 
test1, test2 :: Machine 


-- алфавіт " abc": знаходить перший символ 'a' заміняє на 'b' і зупиняється  
test1 = (1, [ ((1,'a'),(0,'b',No)), ((1,'b'),(1,'b',Rt))
            , ((1,'c'),(1,'c',Rt)), ((1,' '),(1,' ',Rt))])

-- алфавіт " a": невизначена функція переходу в (1,'a') !!! 
test2 = (2,[((2,'a'),(2,'a',Rt)),((2,' '),(1,' ',Rt)),((1,' '),(0,' ',Rt))])

-- будуємо складну машину з найпростіших
-- будуємо машину, що обчислює додавання
-- найпростіші . алфавіт == " #|"
rht, putO, putW :: Machine 
rht  = (1, map (\c->((1,c),(0,c,Rt))) " #|")   -- переміщує вправо
putO = (1, map (\c->((1,c),(0,'|',No))) " #|") -- записує символ '|'
putW = (1, map (\c->((1,c),(0,' ',No))) " #|") -- записує символ ' '

-- складніші машини 
rightO, rightM, main, additionM :: Machine 
rightO = cycleJoin '|' " #|" rht      -- проходить вправо всі '|'
rightM = seqJoin rht rightO           -- вправо завжди і потім вправо всі '|' 
main   = seqJoin (seqJoin putW rightM) putO  -- додавання, коли x>0
additionM = ifJoin '|' " #|" main putW       -- додавання, коли x>=0 

-- приклади побудов машин Тюрінга (обєкти типу Complex)
right, left, copy, addition :: Complex 
-- вправо завжди і потім вправо всі '|'
right = Join [R,While '|' R]
-- вліво завжди і потім вліво всі '|'  
left  = Join [L,While '|' L] 
-- додавання x+y 
addition = If '|' (Join [P ' ',right,P '|']) (P ' ')  
-- копіювання *|.x.| |.y.| ==> *|.x.| |.y+x.| 
copy = Join [While '|' (Join [P ' ',right,right,P '|',left,left,P '|',R])
            ,Join [left,R]
            ]

rightOT, rightMT, mainT, additionMT :: Machine 
rightOT = (2,
  [((1,' '),(2,' ',Rt)),((1,'#'),(2,'#',Rt)),((1,'|'),(2,'|',Rt))
  ,((2,' '),(0,' ',No)),((2,'#'),(0,'#',No)),((2,'|'),(1,'|',No))])
rightMT = (3,
  [((1,' '),(2,' ',Rt)),((1,'#'),(2,'#',Rt)),((1,'|'),(2,'|',Rt))
  ,((2,' '),(0,' ',No)),((2,'#'),(0,'#',No)),((2,'|'),(1,'|',No))
  ,((3,' '),(2,' ',Rt)),((3,'#'),(2,'#',Rt)),((3,'|'),(2,'|',Rt))])
mainT = (5,
  [((1,' '),(0,'|',No)),((1,'#'),(0,'|',No)),((1,'|'),(0,'|',No))
  ,((2,' '),(3,' ',Rt)),((2,'#'),(3,'#',Rt)),((2,'|'),(3,'|',Rt))
  ,((3,' '),(1,' ',No)),((3,'#'),(1,'#',No)),((3,'|'),(2,'|',No))
  ,((4,' '),(3,' ',Rt)),((4,'#'),(3,'#',Rt)),((4,'|'),(3,'|',Rt))
  ,((5,' '),(4,' ',No)),((5,'#'),(4,' ',No)),((5,'|'),(4,' ',No))])  
additionMT = (7,
  [((1,' '),(0,' ',No)),((1,'#'),(0,' ',No)),((1,'|'),(0,' ',No))
  ,((2,' '),(0,'|',No)),((2,'#'),(0,'|',No)),((2,'|'),(0,'|',No))
  ,((3,' '),(4,' ',Rt)),((3,'#'),(4,'#',Rt)),((3,'|'),(4,'|',Rt))
  ,((4,' '),(2,' ',No)),((4,'#'),(2,'#',No)),((4,'|'),(3,'|',No))
  ,((5,' '),(4,' ',Rt)),((5,'#'),(4,'#',Rt)),((5,'|'),(4,'|',Rt))
  ,((6,' '),(5,' ',No)),((6,'#'),(5,' ',No)),((6,'|'),(5,' ',No))
  ,((7,' '),(1,' ',No)),((7,'#'),(1,'#',No)),((7,'|'),(6,'|',No))])