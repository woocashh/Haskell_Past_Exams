import Data.Maybe

data Expr = Number Int |
            Boolean Bool |
            Id String  |
            Prim String |
            Cond Expr Expr Expr |
            App Expr Expr |
            Fun String Expr
          deriving (Eq, Show)

data Type = TInt |
            TBool |
            TFun Type Type |
            TVar String |
            TErr 
          deriving (Eq, Show)

showT :: Type -> String
showT TInt  
  = "Int"
showT TBool 
  = "Bool"
showT (TFun t t') 
  = "(" ++ showT t ++ " -> " ++ showT t' ++ ")"
showT (TVar a) 
  = a
showT TErr  
  = "Type error"

type TypeTable = [(String, Type)]

type TEnv 
  = TypeTable    -- i.e. [(String, Type)]

type Sub 
  = TypeTable    -- i.e. [(String, Type)]  

-- Built-in function types...
primTypes :: TypeTable
primTypes 
  = [("+", TFun TInt (TFun TInt TInt)),
     (">", TFun TInt (TFun TInt TBool)),
     ("==", TFun TInt (TFun TInt TBool)),
     ("not", TFun TBool TBool)]

------------------------------------------------------
-- PART I

-- Pre: The search item is in the table
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp a list
  = tryToLookUp a undefined list

tryToLookUp :: Eq a => a -> b -> [(a, b)] -> b
tryToLookUp a def list
  = fromMaybe def (lookup a list)

-- Pre: The given value is in the table
reverseLookUp :: Eq b => b -> [(a, b)] -> [a]
reverseLookUp v ps
  = [k | (k, v') <- ps, v == v']

occurs :: String -> Type -> Bool
occurs s (TVar x)
  = s == x
occurs s (TFun t1 t2)
  = (occurs s t1) || (occurs s t2)
occurs _ _
  = False

------------------------------------------------------
-- PART II

-- Pre: There are no user-defined functions (constructor Fun)
-- Pre: All type variables in the expression have a binding in the given 
--      type environment
inferType :: Expr -> TEnv -> Type
inferType (Number _) _
  = TInt
inferType (Boolean _) _
  = TBool
inferType (Id s) env
  = lookUp s env
inferType (Prim s) _
  = lookUp s primTypes
inferType (Cond b t f) env
  | inferType b env == TBool && typ == inferType f env = typ
  | otherwise                                          = TErr
  where
    typ = inferType t env
inferType (App f a) env
  = inferApp (App f a) (inferType f env) (inferType a env)

inferApp :: Expr -> Type -> Type -> Type
inferApp (App f a) (TFun t t') typea
  | typea == TErr = TErr
  | typea == t    = t'
  | otherwise     = TErr
inferApp _ _ _
  = TErr
------------------------------------------------------
-- PART III

applySub :: Sub -> Type -> Type
applySub subs (TVar s)
  = tryToLookUp s (TVar s) subs
applySub subs (TFun a b)
  = (TFun (applySub subs a) (applySub subs b))
applySub _ expr
  = expr

unify :: Type -> Type -> Maybe Sub
unify t t'
  = unifyPairs [(t, t')] []

unifyPairs :: [(Type, Type)] -> Sub -> Maybe Sub
unifyPairs [] subs
  = Just subs

unifyPairs ((TInt, TInt) : ts) subs
  = unifyPairs ts subs
unifyPairs ((TBool, TBool) : ts) subs
  = unifyPairs ts subs

unifyPairs ((TVar v, TVar v') : ts) subs
  | v == v'   = unifyPairs ts subs
  | otherwise = unifyPairs (zip (subBoth (fst uts)) (subBoth (snd uts))) 
                           ((v, TVar v') : subs) 
  where 
    subBoth = map (applySub [(v, TVar v')])
    uts = unzip ts
unifyPairs ((TVar v, t) : ts) subs
  | occurs v t = Nothing
  | otherwise  = unifyPairs (zip (subBoth (fst uts)) 
                            (subBoth (snd uts))) ((v, t) : subs) 
  where 
    subBoth = map (applySub [(v, t)])
    uts = unzip ts
unifyPairs ((t, TVar v) : ts) subs
  | occurs v t = Nothing
  | otherwise  = unifyPairs (zip (subBoth (fst uts)) 
                            (subBoth (snd uts))) ((v, t) : subs) 
  where 
    subBoth = map (applySub [(v, t)])
    uts = unzip ts

unifyPairs (((TFun t1 t2), (TFun t1' t2')) : ts) subs
  = unifyPairs ((t1, t1') : ((t2, t2') : ts)) subs

unifyPairs _ _
  = Nothing

------------------------------------------------------
-- PART IV

updateTEnv :: TEnv -> Sub -> TEnv
updateTEnv tenv tsub
  = map modify tenv
  where
    modify (v, t) = (v, applySub tsub t)

combine :: Sub -> Sub -> Sub
combine sNew sOld
  = sNew ++ updateTEnv sOld sNew

-- In combineSubs [s1, s2,..., sn], s1 should be the *most recent* substitution
-- and will be applied *last*
combineSubs :: [Sub] -> Sub
combineSubs 
  = foldr1 combine

inferPolyType :: Expr -> Type
inferPolyType t
  = applySub subs typ
  where
    (subs, typ, names) = inferPolyType' t [] (zipWith (++) (repeat "a") (map show [1..]))


-- You may optionally wish to use one of the following helper function declarations
-- as suggested in the specification. 

inferPolyType' :: Expr -> TEnv -> [String] -> (Sub, Type, [String])
inferPolyType' (Number _) env names
  = ([], TInt, names)
inferPolyType' (Boolean _) env names
  = ([], TBool, names)
inferPolyType' (Prim p) env names
  = ([], lookUp p primTypes, names) 
inferPolyType' (Id x) env (name : names)
  = ([(x, TVar name)], TVar x, names)
inferPolyType' (Fun x e) env (v : vs)
  = (s, TFun (applySub), names)
  where
    tx = TVar a
    b = (x, tx)
    (s, te, vs') = inferPolyType' e (b : env) vs
inferPolyType' (App f e) env (v : vs)
  | (, applySub, vse)
  where
    (sf, tf, vsf) = inferPolyType' f env vs
    (se, te, vse) = inferPolyType' e env vsf
    ms = unify tf (TFun te (TVar a))
 

-- NEEED TO FINISH!!!

-- inferPolyType' :: Expr -> TEnv -> Int -> (Sub, Type, Int)
-- inferPolyType' 
--   = undefined

------------------------------------------------------
-- Monomorphic type inference test cases from Table 1...

env :: TEnv
env = [("x",TInt),("y",TInt),("b",TBool),("c",TBool)]

ex1, ex2, ex3, ex4, ex5, ex6, ex7, ex8 :: Expr
type1, type2, type3, type4, type5, type6, type7, type8 :: Type

ex1 = Number 9
type1 = TInt

ex2 = Boolean False
type2 = TBool

ex3 = Prim "not"
type3 =  TFun TBool TBool

ex4 = App (Prim "not") (Boolean True)
type4 = TBool

ex5 = App (Prim ">") (Number 0)
type5 = TFun TInt TBool

ex6 = App (App (Prim "+") (Boolean True)) (Number 5)
type6 = TErr

ex7 = Cond (Boolean True) (Boolean False) (Id "c")
type7 = TBool

ex8 = Cond (App (Prim "==") (Number 4)) (Id "b") (Id "c")
type8 = TErr

------------------------------------------------------
-- Unification test cases from Table 2...

u1a, u1b, u2a, u2b, u3a, u3b, u4a, u4b, u5a, u5b, u6a, u6b :: Type
sub1, sub2, sub3, sub4, sub5, sub6 :: Maybe Sub

u1a = TFun (TVar "a") TInt
u1b = TVar "b"
sub1 = Just [("b",TFun (TVar "a") TInt)]

u2a = TFun TBool TBool
u2b = TFun TBool TBool
sub2 = Just []

u3a = TFun (TVar "a") TInt
u3b = TFun TBool TInt
sub3 = Just [("a",TBool)]

u4a = TBool
u4b = TFun TInt TBool
sub4 = Nothing

u5a = TFun (TVar "a") TInt
u5b = TFun TBool (TVar "b")
sub5 = Just [("b",TInt),("a",TBool)]

u6a = TFun (TVar "a") (TVar "a")
u6b = TVar "a"
sub6 = Nothing

------------------------------------------------------
-- Polymorphic type inference test cases from Table 3...

ex9, ex10, ex11, ex12, ex13, ex14 :: Expr
type9, type10, type11, type12, type13, type14 :: Type

ex9 = Fun "x" (Boolean True)
type9 = TFun (TVar "a1") TBool

ex10 = Fun "x" (Id "x")
type10 = TFun (TVar "a1") (TVar "a1")

ex11 = Fun "x" (App (Prim "not") (Id "x"))
type11 = TFun TBool TBool

ex12 = Fun "x" (Fun "y" (App (Id "y") (Id "x")))
type12 = TFun (TVar "a1") (TFun (TFun (TVar "a1") (TVar "a3")) (TVar "a3"))

ex13 = Fun "x" (Fun "y" (App (App (Id "y") (Id "x")) (Number 7)))
type13 = TFun (TVar "a1") (TFun (TFun (TVar "a1") (TFun TInt (TVar "a3"))) 
              (TVar "a3"))

ex14 = Fun "x" (Fun "y" (App (Id "x") (Prim "+"))) 
type14 = TFun (TFun (TFun TInt (TFun TInt TInt)) (TVar "a3")) 
              (TFun (TVar "a2") (TVar "a3"))

