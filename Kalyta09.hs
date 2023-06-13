{-# OPTIONS_GHC -Wall #-}
module Kalyta09 where

import Text.ParserCombinators.Parsec
import Data.Char (isDigit)
import Data.List

data Term   =  Nmb Int         -- десяткове число без знаку
            | Var String       -- змінна, довільний ідентифікатор
            | App Term Term    -- операція застосування
            | Abs String Term  --  операція абстракції
           deriving (Show, Eq) 
type Contex = [(String,Term)]

-- Задача 1.a -----------------------------------------
addVar :: String -> [String] -> [String] 
addVar v vs = if null [i|i <- vs, i == v] then v:vs
              else vs

-- Задача 1.b ----------------------------------------- 
delVar :: String -> [String] -> [String]
delVar v vs = [i|i <- vs, i /= v] 

-- Задача 1.c -----------------------------------------
unionV :: [String] -> [String] -> [String]
unionV vs1 vs2 = sort unionLst
       where unionLst = foldr addVar vs1 vs2

-- Задача 1.d ----------------------------------------- 
freeVars    :: Term -> [String]
freeVars (Nmb _) = [] 
freeVars (Var t) = [t]
freeVars (App t1 t2) = unionV (freeVars t1) (freeVars t2)
freeVars (Abs t1 t2) = delVar t1 (freeVars t2)
             

-- Задача 2.a -----------------------------------------
deleteSyn :: String -> Contex -> Contex
deleteSyn nm cnt = [i|i <- cnt, fst i /= nm]

-- Задача 2.b -----------------------------------------
iswfTerm :: Term -> Contex -> Bool
iswfTerm t cnt = null (freeVars t) || (length [tn| tn <- freeVars t, n <- cnt, tn == fst n] == length (freeVars t))

-- Задача 2.c -----------------------------------------
iswfContex :: Contex -> Bool
iswfContex cnt = and [(length cnt - 1) == length (deleteSyn (fst i) cnt) | i <- cnt  ] && and [iswfTerm (snd (cnt !! i)) [cnt !! q | q <- [0.. i - 1]] | i <- [0..length cnt - 1]]
                 
-- Задача 3.a -----------------------------------------
isNumber :: Term -> Bool
isNumber t = null (freeVars t) && isNumberHLPR t

isNumberHLPR :: Term -> Bool
isNumberHLPR (Abs a (Abs b t)) = numAnal [a, b] t
                               where numAnal arr (Var x) = last arr == x
                                     numAnal arr (App (Var z) y) = head arr == z && numAnal arr y
                                     numAnal _ _ = False
isNumberHLPR _ = False


-- Задача 3.b -----------------------------------------
inNumber :: Term -> Term
inNumber(Abs _ (Abs _ t)) = Nmb (makeN t)
                          where makeN (Var _) = 0
                                makeN (App _ lngth) = makeN lngth + 1
                                makeN _ = undefined
inNumber _ = undefined


-- Задача 3.c -----------------------------------------
compress ::  Term -> Term
compress(App t1 t2) = App (if isNumber t1 then inNumber t1
                           else compress t1) (if isNumber t2 then inNumber t2
                                              else compress t2)
compress (Abs t1 t2) = if isNumber (Abs t1 t2) then inNumber (Abs t1 t2)
                      else Abs t1 (if isNumber t2 then inNumber t2
                                  else compress t2)
compress t = t



-- Задача 4 -----------------------------------------
reduce :: Term -> String -> Term -> Term 
reduce (Var w) x s = if (w == x) then s
                else Var w

reduce (App w1 w2) x s = App (reduce w1 x s) (reduce w2 x s)

reduce (Abs z w) x s = if (z == x) then Abs z w
                else if (freeVars s == delVar z (freeVars s) && z /= x) then Abs z (reduce w x s)
                else Abs (newVar (unionV (freeVars s) (freeVars w)) z) (reduce (reduce w z (Var (newVar (unionV (freeVars s) (freeVars w)) z))) x s)

reduce (Nmb n) _ _ = Nmb n

-- Задача 5 -----------------------------------------
evalStep :: Term -> Contex -> Maybe Term   
evalStep t cnt = case evalStepHLPR t cnt of
                Just t1 -> if t == t1 then Nothing else Just t1
                _ -> Nothing


evalStepHLPR :: Term -> Contex -> Maybe Term
evalStepHLPR (App (Abs t t1) t2) _ = Just (reduce t1 t t2)
evalStepHLPR (App t1 t2) t = case evalStepHLPR t1 t of
                        Just p -> if p /= t1 then Just (App p t2) else case evalStepHLPR t2 t of
                                Just b -> Just (App t1 b)
                                _ -> Nothing
                        _ -> case evalStepHLPR t2 t of
                                Just b -> Just (App t1 b)
                                _ -> Nothing
evalStepHLPR (Nmb n) _ = Just(integerTerm n)

evalStepHLPR (Abs s t) c = case evalStepHLPR t (deleteSyn s c) of
                        Just p -> Just (Abs s p)
                        _ -> Nothing
evalStepHLPR (Var p) q = if inCntx p q then Just (chooseTerm p q)
                          else Just (Var p)
                               where inCntx x y = length [i | i <- y, fst i == x] == 1
                                     chooseTerm w z = head [snd i | i <- z, fst i == w]


-- Задача 6 -----------------------------------------
eval :: Int -> Term -> Contex -> Maybe Term 
eval st t ctx = case evalCount st t ctx 0 of
                Just t2 -> Just (compress t2)
                _ -> Nothing
               where evalCount st1 t1 ctx1 n1 = if n1 < st1 then case evalStep t1 ctx1 of
                                                        Just x -> evalCount st1 x ctx1 (n1 + 1)
                                                        _ -> Just t1
                                         else Nothing


-- Задача 7 -----------------------------------------
parseTerm :: String -> Maybe Term 
parseTerm str = case parse pFull "" str of
                Right t -> Just t
                _  -> Nothing

                
pFull :: Parser Term
pFull = do{_ <- spaces; term <- pExp; eof; return term}

pExp :: Parser Term
pExp = do pMltFun

pMltFun :: Parser Term
pMltFun = do {spaces; apps <- many1 pFact; spaces; return (foldl1 App apps)}

pFact :: Parser Term
pFact = do {spaces; helpP "("; term <- pExp; helpP ")";return term} <|> do {var <- many1 letter; spaces; return (Var var)} <|> do {var <- many1 digit; spaces; return (Nmb (read var))} <|> do {helpP "\\"; pId} 

helpP :: String -> Parser ()
helpP s = do {_ <- string s; spaces} 

pId :: Parser Term
pId = do {v <- letter; spaces; dot <- (helpP "." >> pExp) <|> pId; spaces; return (Abs [v] dot)}


        



  

--------------------------------------------------------
-- integerTerm - з числа в вираз
integerTerm :: Int ->  Term
integerTerm n  = (Abs "s" (Abs "z" (buildTerm n))) 
  where buildTerm 0 = Var "z" 
        buildTerm j = (App (Var "s") (buildTerm (j-1)))  

--  New Name -- якщо імя dddname, де ddd-цифри і n-буква, то початкове імя - name 
-- якщо змінна ccc, то її нові імена 0ccc,...,9ccc,09ccc,...
-- цифри на початку - це створення нового імені (problem name capture)
newVar :: [String] -> String -> String
newVar fvs nm = (until (\n -> notElem n fvs) next) (next nm)   -- flip elem fvs
  where next n@(c:_)| c=='9'    = '0':n 
        next (c:cx) | isDigit c = (succ c):cx 
        next n      = '0':n

--------------------------------------------------------
-- Тестові приклади
term0, term0a, term1, term1a, term1b, term1c :: Term
term0 = Abs "s" (Abs "z" (App (Var "s") (App (Var "s") (Var "z")))) 
term0a = Abs "z" (App (Var "s") (App (Var "s") (Var "z")))
term1 = Abs "y" (App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y"))
term1a = App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y")
term1b = Abs "x" (Abs "y" (App (Var "x") (Var "y")))
term1c = Abs "y" (App (Var "x") (Var "y"))

term2, term2a, termAnd, termTest :: Term
term2 = App (App (Abs "f" (Abs "g" (Abs "x" (App (App (Var "f") (Var "x")) (App (Var "g") (Var "x"))))))
                 (Abs "x" (Abs "y" (Var "x")))
            ) 
            (Abs "x" (Abs "y" (Var "x")))
term2a = App (Var "x") (App (Abs "x" (Var "x")) (App (Abs "x" (Var "x")) (Var "z")))
termAnd = Abs "x" (Abs "y" (App (App  (Var "x") (Var "y")) (Var "false")))
termTest = Abs "x" (Abs "x" (Abs "y" (Var "y")))

cont1 :: Contex
cont1 = [("true",Abs "x" (Abs "y" (Var "x")))
        ,("false",Abs "x" (Abs "y" (Var "y")))
        ,("test",Abs "l" (Abs "m" (Abs "n" (App (App (Var "l") (Var "m")) (Var "n")))))
        ,("iszero",Abs "n" (App (App (Var "n") (Abs "x" (Var "false"))) (Var "true")))
        ,("plus",Abs "m" (Abs "n" (Abs "f" (Abs "x" (App (App (Var "n") (Var "f")) (App (App (Var "m") (Var "f")) (Var "x")))))))
        ,("mult",Abs "m" (Abs "n" (Abs "f" (App (Var "m") (App (Var "n") (Var "f"))))))
        ,("pred",Abs "n" (Abs "f" (Abs "x" (App (App (App (Var "n") (Abs "g" (Abs "h" (App (Var "h") (App (Var "g") (Var "f")))))) (Abs "u" (Var "x"))) (Abs "x" (Var "x"))))))
        ,("fixM",Abs "f" (App (Abs "x" (App (Var "f") (Abs "y" (App (App (Var "x") (Var "x")) (Var "y"))))) (Abs "x" (App (Var "f") (Abs "y" (App (App (Var "x") (Var "x")) (Var "y")))))))
        ,("sumR",Abs "r" (Abs "n" (App (App (App (Var "test") (App (Var "iszero") (Var "n"))) (Nmb 0)) (App (App (Var "plus") (Var "n")) (App (Var "r") (App (Var "pred") (Var "n")))))))
        ,("factR",Abs "fact" (Abs "n" (App (App (App (Var "test") (App (Var "iszero") (Var "n"))) (Nmb 1)) (App (App (Var "mult") (Var "n")) (App (Var "fact") (App (Var "pred") (Var "n")))))))
        ,("sum",App (Var "fixM") (Var "sumR"))
        ,("factor",App (Var "fixM") (Var "factR"))
        ]

termS2 :: String 
termS2 = "(\\f g x. f x (g x))   (\\x y .x) (\\x y .x)"