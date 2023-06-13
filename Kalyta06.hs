{-# OPTIONS_GHC -Wall #-}
module Kalyta06 where

import Data.Char(isUpper)
import Data.List

type Grammar    = [Production]         -- КВ-граматика
type Production = (Char,String)        -- Правило виводу
type Predict    = [(Char,String)]      -- Прогнозуюча таблиця
type Control    = [((Char,Char),Int)]  -- Управляюча таблиця 

-- Задача 1 ------------------------------------
addOne :: String -> Char -> String  

addOne st c = if elem c st then st
                 else (st ++ [c])


addAll :: String -> String -> String 
addAll st [] = st
addAll st (w:wd) =  (addAll (addOne st w ) wd)

addWithout :: String -> String -> String 
addWithout st wd = (addAll st (filter (/= '$') wd))

inter :: String -> String -> String 
inter [] _    = []
inter _ []    = []
inter st1 st2 = [x | x <- st1, elem x st2]




-- Задача 2 ------------------------------------
tkPredict :: Predict -> Char -> String 
tkPredict [] _ = "" 
tkPredict (p:pt) n = if ((fst p) == n) then snd p
                   else tkPredict pt n


upPredict :: Predict -> Char -> String -> Predict 
upPredict [] n st = [(n, st)]

upPredict (k:pt) n st = if (n == fst k) then (n, st):pt
  else if (n < fst k) then (n, st) : k : pt
    else k : (upPredict pt n st)



-- Задача 3 ------------------------------------
parse ::  Grammar -> Control -> String -> Maybe [Int]
parse = undefined

step :: Grammar -> Control -> 
       (String, String, Maybe [Int]) -> (String, String, Maybe [Int])
step = undefined


-- Задача 4 ------------------------------------
first :: Predict -> String -> String
first [] _ = error "ERROR: fst table empty" -- empty table erorr
first _ "" = "$" -- __ empty -> $

first pFst (s:st) = if (not (isUpper s)) then [s]
    else (fstNonTerm (s:st)) -- for non terminal first symbol

        where fstNonTerm :: String -> String
              fstNonTerm [] = error "ERROR: empty string"

              fstNonTerm (ss:[]) = case find (\x -> (fst x) == ss) pFst of
                  Just x  -> snd x
                  Nothing -> error "ERROR"
              fstNonTerm (ss:stt) = if (not (elem '$' (getSNDfMYB(find (\x -> (fst x) == ss) pFst)))) then getSNDfMYB(find (\x -> (fst x) == ss) pFst)
                                        else addAll (addWithout "" (getSNDfMYB(find (\x -> (fst x) == ss) pFst))) (first pFst stt)
              
              getSNDfMYB :: Maybe (Char,String) -> String
              getSNDfMYB Nothing = error "ERROR" --nothing error

              getSNDfMYB (Just (_, k)) = k 



-- Задача 5 ------------------------------------
buildingControl :: Grammar -> Predict -> Predict -> Control 
buildingControl [] _ _ = error "ERROR: Grammar is EMPTY"
buildingControl _ [] _ = error "ERROR: first table is EMPTY"
buildingControl _ _ [] = error "EERROR: next table is EMPTY"

buildingControl gr pFst pNxt = sort (concatMap (\x -> createCNTRLlist x pFst pNxt) (zip [0..] gr) )
--make a control list form a production
createCNTRLlist :: (Int, (Production)) -> Predict -> Predict -> [((Char, Char), Int)] 
createCNTRLlist (o, (char, a)) ppFst ppNxt =  if elem '$' (first ppFst a) then [((char, y), o) | y <- tkPredict ppNxt char]
    else [((char, x), o) | x <- (first ppFst a), not (isUpper x)]



-- Задача 6 ------------------------------------
testingLL1 :: Grammar -> Predict -> Predict -> Bool

testingLL1 = undefined

fromGrammar :: Grammar ->  [(Char,[String])]
fromGrammar gr = fDups [(d, rs) | (d, _) <- gr, let rs = rts gr d]

rts :: Grammar -> Char -> [String] --list of rightside otputs of non-terminal stuff
rts gr n = [s | x <- (filter (\y -> (fst y) == n) gr), let s = snd x]

fDups :: (Ord a) => [a] -> [a] -- filters and sorts
fDups = map head . group . sort


testFst :: [String] -> Bool
testFst = undefined

testFollow :: String -> [String] -> Bool
testFollow = undefined

-- Задача 7 ------------------------------------
buildFst :: Grammar -> Predict 
--buildFst [] = error "ERROR: grammar is EMPTY"
buildFst  = undefined

evalFst :: Grammar -> Predict -> Predict 
evalFst gr pFst =

    let npr = pEx gr pFst

        pEx :: Grammar -> Predict -> Predict
        pEx [] pr = pr --if empty grammar
        pEx (g:ggg) pr = pEx ggg (extandFst pr g) 

    in if (pFst == npr) then (npr)
       else (npr)

extandFst :: Predict -> Production -> Predict 
extandFst pFst  (n, rul) = upPredict pFst  n (sort (addAll (first pFst  (rul)) (tkPredict pFst  n)))



-- Задача 8 ------------------------------------
buildNxt :: Grammar -> Predict -> Predict 
--errs 1st
buildNxt [] _ = error "ERROR: grammar is EMPTY"
buildNxt _ [] = error "ERRR: fst table is EMPTY"

--impl
buildNxt gr pFst = fst (until (\x -> not (snd x)) stepNxt ((crtNxt gr), True))
    where
       stepNxt :: (Predict, Bool) -> (Predict, Bool) 
       stepNxt (prr, cnt) = evalNxt (nontermTails gr) pFst (prr, cnt)



--help functions. had to remake
nontermTails :: Grammar -> [(Char,String)]
nontermTails gr = concatMap (tailf1Prod) gr

    where
       tailf1Prod :: Production -> [(Char,String)]
       tailf1Prod (k, st) = [(k, o) | (ind, symb) <- (zip [0..] st), isUpper symb, let o = drop ind st]


evalNxt :: [(Char, String)] -> Predict -> (Predict, Bool) -> (Predict, Bool)

evalNxt tails' pFst (pNxt, _) =
    let npr = pssEx tails' pNxt

        pssEx :: [(Char, String)] -> Predict -> Predict
        pssEx [] pr = pr -- errr

        pssEx ((k, t):tx) pr = pssEx tx (extandNxtOne pFst k pr t)

    in if (pNxt == npr) then (npr, False)
       else (npr, True)


extandNxtOne :: Predict -> Char -> Predict -> String -> Predict
extandNxtOne _ _ _ [] = error "ERROR: tail is EMPTY" -- errr

extandNxtOne pFst n pNxt (o:st) = if elem '$' (first pFst st) then upPredict pNxt o (sort (addAll (tkPredict pNxt o) (addAll (addWithout (first pFst st) "") (tkPredict pNxt n))))
           -- if empty
       else if st == [] then upPredict pNxt o (sort (addAll (tkPredict pNxt o) (tkPredict pNxt n)))
       else upPredict pNxt o (sort (addWithout (tkPredict pNxt o) (first pFst st)))



crtNxt :: Grammar -> Predict
crtNxt gr = 
    let fstnotTerm = fst (head gr)
        grData = fromGrammar gr

    in sort ([(p, "$") | (p, _) <- grData, p == fstnotTerm] ++ [(pp, "") | (pp, _) <- (deleteBy (\_ x -> (fst x) == fstnotTerm) (' ',[""]) grData)])


---------------------Тестові дані ---------------------------
 
gr0, gr1, gr2, gr3, gr4, gr5:: Grammar
--  LL(1)-граматики
gr0 = [('S',"aAS"),('S',"b"), ('A',"a"), ('A',"bSA")]  
gr1 = [('S',"TV"),('T',"d"),('T',"(S)"),('V',"+TV"),('V',"-TV"),('V',"")]  
gr2 = [('E',"TU"),('U',""),('U',"+TU"),('U',"-TU"),
       ('T',"FV"),('V',""),('V',"*FV"),('V',"%FV"),('V',"/FV"),
       ('F',"d"),('F',"(E)")] 
-- не LL(1)-граматики
gr3 = [('S',"aAS"), ('S',"a"),('A',"SbA"),('A',"ba"),('S',"")]
gr4 = [('E',"E+T"),('E',"T"), ('T',"T*F"), ('T',"F"), ('F',"d"),('F',"(E)") ]   
gr5 = [('E',"E+T"), ('E',"E-T"),('E',"T"), 
       ('T',"T*F"), ('T',"T%F"), ('T',"T/F"), ('T',"F"), 
       ('F',"d"),('F',"(E)") ]

-- прогнозуючі таблиці початкових терміналів Fst
pFst0, pFst1, pFst2, pFst3, pFst4, pFst5 :: Predict
pFst0 = [('A',"ab"),('S',"ab")]
pFst1 = [('S',"(d"),('T',"(d"),('V',"$+-")]
pFst2 = [('E',"(d"),('F',"(d"),('T',"(d"),('U',"$+-"),('V',"$%*/")]
pFst3 = [('A',"ab"),('S',"$a")]
pFst4 = [('E',"(d"),('F',"(d"),('T',"(d")]
pFst5 = [('E',"(d"),('F',"(d"),('T',"(d")]

-- прогнозуючі таблиці наступних терміналів Nxt
pNxt0, pNxt1, pNxt2, pNxt3, pNxt4, pNxt5 :: Predict
pNxt0 = [('A',"ab"),('S',"$ab")]
pNxt1 = [('S',"$)"),('T',"$)+-"),('V',"$)")]
pNxt2 = [('E',"$)"),('F',"$%)*+-/"),('T',"$)+-"),('U',"$)"),('V',"$)+-")]
pNxt3 = [('A',"$ab"),('S',"$b")]
pNxt4 = [('E',"$)+"),('F',"$)*+"),('T',"$)*+")]
pNxt5 = [('E',"$)+-"),('F',"$%)*+-/"),('T',"$%)*+-/")]   

-- управляючі таблиці 
ctl0, ctl1, ctl2 :: Control 
ctl0 = [(('A','a'),2),(('A','b'),3),(('S','a'),0),(('S','b'),1)]
ctl1 = [(('S','('),0),(('S','d'),0),(('T','('),2),(('T','d'),1),
        (('V','$'),5),(('V',')'),5),(('V','+'),3),(('V','-'),4)]
ctl2 = [(('E','('),0),(('E','d'),0),(('F','('),10),(('F','d'),9),
        (('T','('),4),(('T','d'),4),(('U','$'),1),(('U',')'),1),
        (('U','+'),2),(('U','-'),3),(('V','$'),5),(('V','%'),7),
        (('V',')'),5),(('V','*'),6),(('V','+'),5),(('V','-'),5),(('V','/'),8)]

