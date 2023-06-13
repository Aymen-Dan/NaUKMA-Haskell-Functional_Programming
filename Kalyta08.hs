{-# OPTIONS_GHC -Wall #-}
module Kalyta08 where

import Data.List
import Data.Maybe(fromMaybe)
--import qualified Text.ParserCombinators.Parsec as P

data RE = Null   |
          Term Char |
          Seq RE RE |
          Alt RE RE |
          Rep RE    |
          Plus RE   |
          Opt RE
        deriving (Eq, Show)

type State = Int
data Label = C Char | Eps deriving (Eq, Ord, Show)
type Transition = (State, State, Label)
type Automation = (State, [State], [Transition])

type MetaState = [State]
type MetaTransition = (MetaState, MetaState, Label)

-- Задача 1 -----------------------------------------
simplify :: RE -> RE   
simplify Null = Null
simplify (Term re) = Term re
simplify (Seq rE1 rE2) = Seq (simplify rE1) (simplify rE2)
simplify (Alt rE1 rE2) = Alt (simplify rE1) (simplify rE2)
simplify (Rep re) = Rep (simplify re)
simplify (Plus re) = Seq (simplify re) (Rep (simplify re))
simplify (Opt re) = Alt (simplify re) Null


-- Задача 2 -----------------------------------------
isTerminal :: Automation -> State -> Bool 
isTerminal (_, aut, _) s = elem s aut

isEssential :: Automation -> State -> Bool 
isEssential a@(_, _, aut) s = if (isTerminal a s) then True
  else (charCIsPres aut s) 

charCIsPres :: [Transition] -> State -> Bool
charCIsPres [] _ = False
charCIsPres ((x, _, C _) : trans) stt = (charCIsPres trans stt) || (x == stt)
charCIsPres ((_, _, _) : trans) stt = charCIsPres trans stt 


-- Задача 3 -----------------------------------------
transitionsFrom :: Automation -> State -> [Transition]
transitionsFrom aut s = filter (\x -> transCheck s x) (getTrans aut)

transCheck :: State -> Transition -> Bool
transCheck s (x1, _, _) = if (s == x1) then True
  else False

getTrans :: Automation -> [Transition]
getTrans (_, _, trans) = trans


-- Задача 4 -----------------------------------------
labels :: [Transition] -> [Label]
labels trx = nub (map getLbl (filter (\x -> getLbl x /= Eps) trx))
   where getLbl :: Transition -> Label
         getLbl (_, _, l) = l


-- Задача 5 -----------------------------------------
acceptsDA :: Automation -> String -> Bool
acceptsDA (bgn, fin, trans) st = acceptsDA' bgn st

    where acceptsDA' :: State -> String -> Bool
          acceptsDA' now [] = elem now fin 
          acceptsDA' now (s : ss) = if elem now fin then False
            else case find (\x -> findNeededLabel x now s) trans of
                Just (_, nxt, _) -> acceptsDA' nxt ss 
                Nothing         -> False

findNeededLabel :: Transition -> State -> Char -> Bool
findNeededLabel (s, _, C c) st cc = (c == cc) && (s == st) 
findNeededLabel (_,_,_) _ _ = False                

-- Задача 6 -----------------------------------------
stStep  :: Automation -> State -> Label -> [State]
stStep (_, _, naut) st mc = [to | (from, to, l) <- naut, from == st, l == mc]

setStep :: Automation -> [State] -> Label -> [State]
setStep naut bs mc = reverse (nub (concatMap (\x -> stStep naut x mc) bs))


closure :: Automation -> [State] -> [State]
closure naut ss = nub (sort ((closureHLPR naut ss []) ++ ss))

closureHLPR :: Automation ->  [State] -> [State] -> [State]
closureHLPR naut [] vstd = vstd
closureHLPR naut ws vstd = let ans = filter (\x -> notElem x vstd) (setStep naut ws Eps)
  in closureHLPR naut ans (ans ++ vstd)
    

-- Задача 7 -----------------------------------------
accepts :: Automation -> String -> Bool
accepts aut st = acceptsHLPR aut st (getBgnStt aut)

acceptsHLPR :: Automation -> [Char] -> MetaState -> Bool
acceptsHLPR _ _ [] = False
acceptsHLPR aut [] st = any (\st1 -> isTerminal aut st1) st
acceptsHLPR aut (x : xx) nowStts = acceptsHLPR aut xx nowStt
   where nowStt = setStep aut stts (C x)
         stts = closure aut nowStts

getBgnStt :: Automation -> MetaState
getBgnStt (aut, _, _) = [aut]


-- Задача 8 -----------------------------------------
makeNDA :: RE -> Automation
makeNDA re = (1, [2], sort transitionsAll)
  where (transitionsAll, _) = make (simplify re) 1 2 3

make :: RE -> Int -> Int -> Int -> ([Transition], Int) 
make Null beg fin nxt = ([(beg, fin, Eps)], nxt)

make (Term c) beg fin nxt = ([(beg, fin, C c)], nxt)

make (Seq lft rt) beg fin nxt = ((nxt, nxt + 1, Eps) : trans ++ trans1, st1)
      where (trans, st) = make lft beg nxt (nxt + 2)
            (trans1, st1) = make rt (nxt + 1) fin st
                             
make (Alt lft rt) beg fin nxt = ((beg, nxt, Eps) : (nxt + 1, fin, Eps) : (beg, nxt + 2, Eps) : (nxt + 3, fin, Eps) : trans ++ trans1, st1)
      where (trans, st) = make lft nxt (nxt + 1) (nxt + 4)
            (trans1, st1) = make rt (nxt + 2) (nxt + 3) (st)

make (Rep rt) beg fin nxt = ((beg, nxt, Eps):(beg, fin, Eps) : (nxt + 1, fin, Eps):(nxt + 1, nxt, Eps) : trans, st)
      where (trans, st) = make rt nxt (nxt + 1) (nxt + 2)  

make (Plus _) _ _ _ = undefined
make (Opt _) _ _ _ = undefined


-- Задача 9 -----------------------------------------
parseReg :: String -> Maybe RE 
parseReg = undefined


-- Задача 10 -----------------------------------------
makeDA' :: Automation -> (MetaState, [MetaState], [MetaTransition])
makeDA' a = (head a1, a1, a3)
     where (a1, _, a3) = until cond (step a) ([], [iss a], [])
           iss tt@(tt1, _, _) = filter (\z -> isEssential tt z) (closure a [tt1])

           cond (_, b, _) = null b
           step aut (g, b, mtr) = addStates iMm mxx mlz l
                     where addStates (gg, bb, mm) ms ml ll = if null ml then (gg, bb, mm)
                                else addStates (gg, bb', mm') ms (tail ml) (tail ll) 

                                     where bb' = if elem h bb || elem h gg then bb else bb ++ [h]
                                           mm' = mm ++ [(ms, h, head ll)]
                                           h = head ml 
                           iMm = (g ++ [mxx], tail b, mtr)
                           mxx = head b                       
                           mlz = map (\z -> filter (isEssential aut) z) bMlz
                           l = nub (concatMap (\z -> labels (transitionsFrom aut z)) mxx)
                           bMlz = map (\z -> closure aut (setStep aut mxx z)) l
                           


makeDA :: Automation -> Automation
makeDA nda = (1, m, sortBy myCompare n)
     where m = [z + 1 | z <- [0..(length ls) - 1], (not . null) (intersect (ls !! z) m)]
           (_, l2, l3) = makeDA' nda
           ls = l2
           lt = l3

           
           myCompare (q1, p1, _) (q2, p2, _) = if q1 > q2 then GT
                else if q1 < q2 then LT
                else compare p1 p2


           n = [(ind ls i1, ind ls i2, i3) | (i1, i2, i3) <- lt]

           ind ss indx = (fromMaybe (-2) $ elemIndex indx ss) + 1

-------------------------------------------------------
-- showRE - Функція може бути корисною при тестуванні
showRE :: RE -> String
showRE (Seq re re') = showRE re ++ showRE re'
showRE (Alt re re') = "(" ++ showRE re ++ "|" ++ showRE re' ++ ")"
showRE (Rep re)     = showRE' re ++ "*"
showRE (Plus re)    = showRE' re ++ "+"
showRE (Opt re)     =  showRE' re ++ "?"
showRE re           = showRE' re

showRE' :: RE -> String
showRE' Null      = ""
showRE' (Term c)  = [c]
showRE' (Alt re re') = showRE (Alt re re')
showRE' re        = "(" ++ showRE re ++ ")"

--------------------------------------------------------
-- Тестові приклади
reFigureS, re1S, re2S, re3S, re4S, re5S, re6S :: String
reFigureS = "(a|b)*c"
re1S = "(x|y)(1|2)"
re2S = "x'*"
re3S = "(ab|c)*"
re4S = "(a?)a"
re5S = "(ab)?d+"
re6S = "c?*"

reFigure, re1, re2, re3, re4, re5, re6 :: RE
reFigure = Seq (Rep (Alt (Term 'a') (Term 'b'))) (Term 'c')
re1 = Seq (Alt (Term 'x') (Term 'y')) (Alt (Term '1') (Term '2'))
re2 = Seq (Term 'x') (Rep (Term '\''))
re3 = Rep (Alt (Seq (Term 'a') (Term 'b')) (Term 'c'))
re4 = Seq (Opt(Term 'a')) (Term 'a')
re5 = Seq (Opt (Seq (Term 'a') (Term 'b'))) (Plus (Term 'd'))
re6 = Rep (Opt (Term 'c'))

ndaFigure, nda1, nda2, nda3, nda4, nda5, nda6, ndaTest :: Automation
daFigure, da1, da2, da3, da4, da5, da6 :: Automation
ndaFigure
  = (1,[2],[(1,3,Eps),(1,5,Eps),(3,4,Eps),(4,2,C 'c'),(5,7,Eps),
            (5,9,Eps),(6,3,Eps),(6,5,Eps),(7,8,C 'a'),(8,6,Eps),
            (9,10,C 'b'),(10,6,Eps)])
daFigure
  = (1,[2],[(1,1,C 'a'),(1,1,C 'b'),(1,2,C 'c')])

nda1 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,9,Eps),(4,11,Eps),
            (5,6,C 'x'),(6,3,Eps),(7,8,C 'y'),(8,3,Eps),(9,10,C '1'),
            (10,2,Eps),(11,12,C '2'),(12,2,Eps)])
da1 = (1,[3],
     [(1,2,C 'x'),(1,2,C 'y'),(2,3,C '1'),(2,3,C '2')])

nda2 = (1,[2],[(1,3,C 'x'),(3,4,Eps),(4,2,Eps),(4,5,Eps),(5,6,C '\''),
            (6,2,Eps),(6,5,Eps)])
da2 = (1,[2],
     [(1,2,C 'x'),(2,2,C '\'')])

nda3 = (1,[2],[(1,2,Eps),(1,3,Eps),(3,5,Eps),(3,7,Eps),(4,2,Eps),
            (4,3,Eps), (5,9,C 'a'),(6,4,Eps),(7,8,C 'c'),(8,4,Eps),
            (9,10,Eps),(10,6,C 'b')])
da3 = (1,[1],
     [(1,1,C 'c'),(1,2,C 'a'),(2,1,C 'b')])

nda4 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,2,C 'a'),(5,6,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps)])
da4 = (1,[2,3],[(1,2,C 'a'),(2,3,C 'a')])

nda5 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,11,C 'd'),(5,9,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps),(9,10,Eps),(10,6,C 'b'),
            (11,12,Eps),(12,2,Eps),(12,13,Eps),(13,14,C 'd'),
            (14,2,Eps),(14,13,Eps)])
da5 = (1,[2],[(1,2,C 'd'),(1,3,C 'a'),(2,2,C 'd'),(3,4,C 'b'),
            (4,2,C 'd')])

nda6 = (1,[2], [(1,2,Eps),(1,3,Eps),(3,5,Eps),(5,6, C 'c'), (6,4,Eps), 
                (4,2,Eps), (3,7,Eps), (7,8,Eps), (8,4,Eps), (4,3,Eps)]) 
da6 = (1,[1], [(1,1, C 'c')])

ndaTest = (1, [1], [(1,2, C 'a'), (1,4, Eps), (1,3, C 'b'), (2,3, Eps),
              (3,5, Eps), (3,4, C 'a'), (4,4, Eps), (4,1, Eps), (5,2, Eps), (5,4,Eps)] )
