{-# OPTIONS_GHC -Wall #-}
module Kalyta10 where

-- розглядаємо лише цілі дані: скаляри  і масиви  
--------------------------------------------------------------------
import Data.List

type Id    = String
data Value = I Int | A [(Int, Int)]  deriving (Eq, Show)
data Op    = Add | Minus | Mul | Less | Equal | Index  deriving (Eq, Show)

data Exp = Const Int 
         | Var Id 
         | OpApp Op Exp Exp 
         | Cond Exp Exp Exp 
         | FunApp Id [Exp] 
         deriving (Eq, Show)

data Stmt = Assign Id Exp 
          | AssignA Id Exp Exp 
          | If Exp Stmt Stmt 
          | While Exp Stmt 
          | Call Id [Exp] 
          | Block [VarDef] [Stmt]
          deriving (Eq, Show)

data VarDef  =  Arr Id | Int Id deriving (Eq, Show)

type FunDef  =  (Id, ([VarDef], Exp))
-- функції повертають лише цілі скалярні дані, не використовують глобальні дані (чисті!!)
type ProcDef = (Id, ([VarDef], Stmt))
type Program = ([VarDef], [FunDef], [ProcDef])

type StateP  = [(Id, Value)]  -- стек даних

data Type    = At | It  deriving (Eq, Show)
type FunEnv  = [(Id,[Type])]
type ProcEnv = [(Id,[Type])]
type VarEnv  = [(Id,Type)]

-- Задача 1 ------------------------------------
updateValue :: Eq a => a -> b -> [(a,b)] -> [(a,b)]

--неможливо використати abs як назву аргументу
updateValue a b abss = case findIndex (\(al, _) -> al == a) abss of
  Nothing -> abss ++ [(a, b)]
  Just n -> let (lft, rght) = splitAt n abss in lft ++ [(a, b)] ++ tail rght

-- Задача 2 ------------------------------------
updateArray :: Value -> Value -> Value -> Value
updateArray (A a) (I i) (I v) = A (updateValue i v a)
updateArray _ _ _ = error "Bad arguments for updateArray function"

-- Задача 3 ------------------------------------
applyOp :: Op -> Value -> Value -> Value 
applyOp Add (I v1) (I v2) = I (v1 + v2)
applyOp Minus (I v1) (I v2) = I (v1 - v2)
applyOp Mul (I v1) (I v2) = I (v1 * v2)
applyOp Less (I v1) (I v2) = if (v1 < v2) then I v1
                             else I v2
applyOp Equal (I v1) (I v2) = if (v1 == v2) then I 1
                              else I 0
applyOp Index (A v1) (I v2) = maybe (I 0) (\(_, x) -> I x) (find (\(v, _) -> v == v2) v1)
applyOp _ _ _ = error "Bad arguments for applyOp function"


-- Задача 4 ------------------------------------
evExp ::  Exp -> [FunDef] -> StateP -> Value 
evExp (Const c) _ _ = I c
evExp (Var v) _ st = lookUp v st
evExp (Cond v e1 e2) dfx st = evExp res dfx st where res = if evExp v dfx st /= I 0 then e1
                                                           else e2
evExp (OpApp e e1 e2) dfx st = applyOp e (evExp e1 dfx st) (evExp e2 dfx st)

evExp (FunApp f es) dfx st = undefined

getId :: VarDef -> Id
getId (Arr vars) = vars
getId (Int vars) = vars     


evArgs :: [Exp] -> [FunDef] ->StateP -> [Value]  
evArgs ex dfx st = map (\e -> evExp e dfx st) ex


-- Задача 5 ------------------------------------
evStmt :: Stmt -> [FunDef] -> [ProcDef] -> StateP -> StateP

evStmt (Assign v s) dfx _ st = updateValue v (evExp s dfx st) st

evStmt (AssignA v e1 e2) dfx _ st = updateValue v (updateArray arr x1 x2) st
                                  where x1 = evExp e1 dfx st
                                        x2 = evExp e2 dfx st
                                        arr = lookUp v st
                                       

evStmt (If s stm1 stm2) dfx dpx st = evStmt (if a /= 0 then stm1 else stm2) dfx dpx st
                                where
                                     I a = evExp s dfx st

evStmt (While s stmt) dfx dpx st = until (\st' -> evExp s dfx st' == I 0) (evStmt stmt dfx dpx) st

evStmt (Call s stmt) dfx dpx st = evStmtHLPR ids newStmt
                                    where (varDefs, stmt1) = lookUp s dpx
                                          ids = map getId varDefs
                                          vals = zip ids (map (\x -> evExp x dfx st) stmt)
                                          new = st ++ vals
                                          newStmt = evStmt stmt1 dfx dpx new
                                       

evStmt (Block s stmts) dfx dpx st = evStmtHLPR ids newStmt
                                       where ids = map fst vals
                                             vals = map initv s
                                             new = st ++ vals
                                             newStmt = foldl (\a x-> evStmt x dfx dpx a) new stmts


evStmtHLPR :: [Id] -> StateP -> StateP                            
evStmtHLPR ids newStmt = filter (\(x, _) -> x `notElem` ids) newStmt


-- Задача 6 ------------------------------------
iswfExp :: Exp -> VarEnv -> FunEnv -> Maybe Type  
iswfExp (Const _) _ _ = Just It
iswfExp (Var n) ve _ = case find (\(a, _) -> a == n) ve of
  Nothing -> Nothing
  Just (_, x) -> Just x 
iswfExp (OpApp e e1 e2) ve fe = isJustWfOp e [iswfExp e1 ve fe, iswfExp e2 ve fe]
iswfExp (Cond e e1 e2) ve fe = case [iswfExp e ve fe, iswfExp e1 ve fe, iswfExp e2 ve fe] of
  [Just It, Just It, Just It] -> Just It
  [Just It, Just At, Just At] -> Just At
  _ -> Nothing
iswfExp (FunApp n ef) ve fe = let ft = map Just (lookUp n fe)
                                  eff = map (\x -> iswfExp x ve fe) ef
                              in if ft == eff then Just It else Nothing 


isJustWfOp :: Op -> [Maybe Type] -> Maybe Type
isJustWfOp Add   [Just It, Just It] = Just It
isJustWfOp Minus [Just It, Just It] = Just It
isJustWfOp Mul   [Just It, Just It] = Just It
isJustWfOp Less  [Just It, Just It] = Just It
isJustWfOp Equal [Just It, Just It] = Just It
isJustWfOp Index [Just At, Just It] = Just It
isJustWfOp _ _      = Nothing



-- Задача 7 ------------------------------------
iswfStmt :: Stmt -> VarEnv -> FunEnv -> ProcEnv -> Bool
iswfStmt (Assign n is) ve fe _ = lookup n ve == Just It && lookup n ve == iswfExp is ve fe

iswfStmt (AssignA n is1 is2) ve fe _ = iswfAssignA [f, s, t]
                                       where Just f = lookup n ve
                                             Just s = iswfExp is1 ve fe
                                             Just t = iswfExp is2 ve fe

iswfStmt (If ex is1 is2) ve fe pe = iswfExp ex ve fe == Just It && iswfStmt is1 ve fe pe && iswfStmt is2 ve fe pe
iswfStmt (While cond s1) ve fe pe = iswfExp cond ve fe == Just It && iswfStmt s1 ve fe pe
iswfStmt (Call n es) ve fe pe =  map (\a -> iswfExp a ve fe) es == map Just (lookUp n pe) 
iswfStmt (Block dfs stts) ve fe pe = all (\a -> iswfStmt a (ve ++ map rwrtVarDefs dfs) fe pe) stts 


rwrtVarDefs :: VarDef -> (Id, Type)
rwrtVarDefs (Arr a) = (a, At)
rwrtVarDefs (Int a) = (a, It)



-- Задача 8 ------------------------------------
iswfFunDef :: FunDef -> FunEnv -> Bool
iswfFunDef df fe = let (n, (t, ex)) = df
                       tID = (n, map ttype t)
                       ve = map rwrtVarDefs t
                   in elem tID fe && iswfExp ex ve fe /= Nothing


iswfProcDef :: ProcDef -> VarEnv -> FunEnv -> ProcEnv -> Bool
iswfProcDef dp ve fe pe = let (pn, (pt, st)) = dp
                              newPt = map ttype pt
                              newPe = updateValue pn newPt pe
                              newVe = map rwrtVarDefs pt ++ ve
                          in elem (pn, newPt) newPe && iswfStmt st newVe fe newPe


ttype :: VarDef -> Type
ttype (Arr _) = At
ttype (Int _) = It



-- Задача 9 ------------------------------------
iswfProgram :: Program -> Bool
iswfProgram pr = iswfPrgrmHLPR pr


iswfPrgrmHLPR :: ([VarDef], [FunDef], [ProcDef]) -> Bool
iswfPrgrmHLPR (vd, fd, pd) =  all (\x -> iswfFunDef x fe) fd && all (\x -> iswfProcDef x ve fe pe) pd && procMain pe && onlyN ve fe pe
                           where ve = map rwrtVarDefs vd
                                 fe = makeFEnv fd
                                 pe = makeFEnv pd
                                 procMain pm = any (\(pname, _) -> pname == "main") pm

onlyN :: VarEnv -> FunEnv -> ProcEnv -> Bool
onlyN ve fe pe = check ((getAllNames ve) ++ (getAllNames fe) ++ (getAllNames pe))
                 where getAllNames an = map (\(name, _) -> name) an

check :: Eq a => [a]  -> Bool
check [] = True
check (a:arr) =  notElem a arr && check arr

                           
makeFEnv :: [(Id, ([VarDef], a))] -> FunEnv
makeFEnv fd = map initFenv fd 
               where initFenv (fn, (fargs, _)) = (fn, map ttype fargs)




--- Допоміжні функції -----------------------------
lookUp :: Eq a => a -> [(a,b)] -> b
-- Передумова: Пара з ключом a є в списку пар abx
lookUp a abx = maybe (error "lookUp") id (lookup a abx) 

-- формує початкове значення змінної
initv :: VarDef -> (Id, Value)
initv (Arr v) = (v, A [])
initv (Int v) = (v, I 0) 

-- Реалізація виконання програми 
evProgram :: Program -> StateP 
evProgram (dvx, dfx, dpx) = 
   let sb = map initv dvx 
       ( _, s) = lookUp "main" dpx      
   in  evStmt s dfx dpx sb   

--  iswfOp o ts - перевіряє коректність типів операндів ts 
--     бінарної операції o і формує тип результату Just t або Nothing  
iswfOp :: Op -> [Type] -> Maybe Type 
iswfOp Add   [It,It] = Just It
iswfOp Minus [It,It] = Just It
iswfOp Mul   [It,It] = Just It
iswfOp Less  [It,It] = Just It
iswfOp Equal [It,It] = Just It
iswfOp Index [At,It] = Just It
iswfOp _      _      = Nothing

--  iswfCond ts - перевіряє коректність  типів операндів ts
--     умовного виразу і формує тип результату Just t або Nothing 
iswfCond :: [Type] -> Maybe Type 
iswfCond [It,It,It] = Just It
iswfCond [It,At,At] = Just At
iswfCond _          = Nothing 

-- iswfAssignA ts перевіряє коректність  типів операндів ts
--   операції присвоювання значення елементу масива 
iswfAssignA :: [Type] -> Bool
iswfAssignA [At,It,It] = True 
iswfAssignA _          = False  

---- Дані для тестування  -----------------------
-- Стан для тестування
sampleState :: StateP
sampleState = [("x",I 5),("y",I 2),("a", A [(2,3),(0,4), (1,2)])]

varEnv :: VarEnv 
varEnv = [("x",It), ("y",It), ("a",At)]

-- Функція максимум двох чисел 
-- func biggest(m,n)= (m<n ? n : m)
biggest :: FunDef
biggest =("biggest",
          ([Int "m", Int "n"], 
           Cond (OpApp  Less (Var "m") (Var "n"))  (Var "n")  (Var "m")                                                                
           )
         )
-- Функція, що обчислює число Фібоначчі
-- func fib(n) = (n<3 ? 1 : fib(n-1) + fib(n-2))
fib :: FunDef
fib = ("fib",
       ([Int "n"], 
        Cond (OpApp Less (Var "n") (Const 3))
             (Const 1)
             (OpApp Add (FunApp "fib" [OpApp Minus (Var "n") (Const 1)])
                        (FunApp "fib" [OpApp Minus (Var "n") (Const 2)]))
       )
      )

-- Функція - сума елементів масиву 0..n ...
-- func sumA(a[],n) = (n<0 ? 0 : a[n] + sumA (a,n-1))
sumA :: FunDef
sumA = ("sumA",
        ([Arr "a", Int "n"],
         Cond (OpApp Less (Var "n") (Const 0)) 
              (Const 0)
              (OpApp Add (OpApp Index (Var "a") (Var "n"))
                         (FunApp "sumA" [Var "a", OpApp Minus (Var "n")(Const 1)])
              )
        )
       )

funEnv :: FunEnv
funEnv = [("biggest",[It,It]),("fib", [It]),("sumA",[At,It])]

-- Приклад оператору - блоку 
sampleBlock :: Stmt 
sampleBlock = Block [Arr "b"]
                 [AssignA "b" (Const 0) (Const 9), AssignA "b" (Const 2) (Const 5),
                  AssignA "b" (Const 3) (Const 7), AssignA "b" (Const 5) (Const 1),
                  Call "sumA1" [Var "b", Const 5]
                 ]

-- Процедура - додавання двох чисел...
-- proc gAdd(x,y) gSum = x + y 
gAdd :: ProcDef
gAdd = ("gAdd", 
        ([Int "x", Int "y"], 
         Assign "gSum" (OpApp Add (Var "x") (Var "y"))
        )
       )

-- Процедура - сума елементів масиву 0..n ...
-- proc sumA1(a[],n) {i;limit;
--      sA=0; i=0; limit=n+1;
--      while (i<limit){sA=sA+a[i]; i=i+1}
--                   }
sumA1 :: ProcDef
sumA1 = ("sumA1",
         ([Arr "a", Int "n"], 
          Block [Int "i", Int "limit"] 
            [Assign "sA" (Const 0), Assign "i" (Const 0),
             Assign "limit" (OpApp Add (Var "n") (Const 1)),
             While (OpApp Less (Var "i") (Var "limit"))
                   (Block [] 
                     [Assign "sA" (OpApp Add (Var "sA")
                                  (OpApp Index (Var "a") (Var "i"))),
                      Assign "i" (OpApp Add (Var "i") (Const 1))
                     ]
                   )
            ]
         )
        )

procEnv :: ProcEnv 
procEnv = [("gAdd",[It,It]),("sumA1",[At,It])]

-- Повні програми
-- gSum; 
-- proc gAdd(x,y) gSum = x + y 
-- proc main() call gAdd(5,10)   
pr1 :: Program
pr1 = ([Int "gSum"], [], [gAdd, ("main",([],Call "gAdd" [Const  5, Const 10]))])

-- sA
-- proc sumA1(a[],n) {i;limit; .... } 
-- proc main() {b[]; b[0]=9; b[2]=5; b[3]=7; b[5]=1;
--                   call sumA1 (b,5)
--             }

pr2 :: Program
pr2 = ([Int "sA"], [], 
       [sumA1, 
        ("main",([], sampleBlock))
       ])
