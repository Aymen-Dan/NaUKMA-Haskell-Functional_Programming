{-# OPTIONS_GHC -Wall #-}
module Kalyta03 where

type Grammar = [Production]
type Production = (Char,String)
-- Граматика - список продукцій.
--   нетермінал першої - початковий

-- Лівосторонній вивід - послідовність слів  [String] з іншого боку
--     послідовність номерів правил, які при цьому застосовувались [Int]
type DerivationS = [String] 
type DerivationR = [Int] 

-- Задача 1 -----------------------------------------
isGrammar ::  Grammar -> Bool
isGrammar = undefined

-- Задача 2.a ---------------------------------------
allTerm :: Grammar -> String
allTerm = undefined

-- Задача 2.b ---------------------------------------
allNotT :: Grammar -> String
allNotT = undefined

-- Задача 2.c ---------------------------------------
newMinN :: Grammar -> Char
newMinN = undefined

-- Задача 3.a -----------------------------------------
buildGen :: Grammar -> String
buildGen = undefined

-- Задача 3.b -----------------------------------------
buildAcc :: Grammar -> String
buildAcc = undefined

-- Задача 3.c -----------------------------------------
reduce :: Grammar -> Grammar
reduce = undefined

-- Задача 4.a -----------------------------------------
findLeftR :: Grammar -> String
findLeftR = undefined

-- Задача 4.b -----------------------------------------
deleteLeftR :: Grammar -> Char -> Grammar
deleteLeftR = undefined

-- Задача 5.a -----------------------------------------
isFact :: Grammar -> Char -> Bool
isFact = undefined

-- Задача 5.b -----------------------------------------
deleteFact :: Char -> String -> Grammar -> Grammar
deleteFact = undefined

-- Задача 6.a -----------------------------------------
isLeftDerivationS :: Grammar -> DerivationS -> Bool
isLeftDerivationS = undefined

-- Задача 6.b -----------------------------------------
isLeftDerivationR :: Grammar -> DerivationR -> Bool
isLeftDerivationR = undefined

-- Задача 7 -----------------------------------------
fromLeftR :: Grammar -> DerivationR -> DerivationS
fromLeftR = undefined

--------------------------------------------------------
--  тестові дані 
gr0, gr1, gr1e, gr2 :: Grammar   
gr0 = [('S',"aAS"), ('S',"a"),('A',"SbA"),('A',"ba")]
gr1 = [ ('S',"aSa"), ('S',"bSd"), ('S',"c"), ('S',"aSb"), ('D',"aC") 
      , ('A',"cBd"), ('A',"aAd"),('B',"dAf"),('C',"cS"), ('C',"a")]
gr1e = [('S',"aAS"), ('S',"a"),('a',"SbA"),('A',"ba"),('S',"")]
gr2 = [('E',"E+T"),('E',"T"), ('T',"T*F"), ('T',"F"), ('F',"d"),('F',"(E)") ]   

gr0S, gr0Se :: [String]
gr0S = ["S", "aAS", "aSbAS", "aabAS", "aabbaS", "aabbaa"]
gr0Se = ["S", "aAS", "aSbAS", "aabAS", "aabbaS", "aabba"]

gr0R :: DerivationR
gr0R = [0, 2, 1, 3, 1]



