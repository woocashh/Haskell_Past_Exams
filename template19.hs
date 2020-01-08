module SOL where

import Data.List
import Data.Maybe

import Types
import TestData

printF :: Formula -> IO()
printF
  = putStrLn . showF
  where
    showF (Var v)
      = v
    showF (Not f)
      = '!' : showF f
    showF (And f f')
      = "(" ++ showF f ++ " & " ++ showF f' ++ ")"
    showF (Or f f')
      = "(" ++ showF f ++ " | " ++ showF f' ++ ")"

--------------------------------------------------------------------------
-- Part I

-- 1 mark
lookUp :: Eq a => a -> [(a, b)] -> b
-- Pre: The item being looked up has a unique binding in the list
lookUp 
  = ((.).(.)) fromJust lookup

-- 3 marks
vars :: Formula -> [Id]
vars frm
  = (sort.nub) (vars' frm [])
  where
    vars' (Var nm) nms
      = (nm : nms)
    vars' (Not fr) nms
      = vars' fr nms
    vars' (Or fr1 fr2) nms
      = (vars' fr1 []) ++ (vars' fr2 []) ++ nms 
    vars' (And fr1 fr2) nms
      = (vars' fr1 []) ++ (vars' fr2 []) ++ nms 

-- 1 mark
idMap :: Formula -> IdMap
idMap frm
  = zip (vars frm) [1..] 

--------------------------------------------------------------------------
-- Part II

-- An encoding of the Or distribution rules.
-- Both arguments are assumed to be in CNF, so the
-- arguments of all And nodes will also be in CNF.
distribute :: CNF -> CNF -> CNF
distribute a (And b c)
  = And (distribute a b) (distribute a c)
distribute (And a b) c
  = And (distribute a c) (distribute b c)
distribute a b
  = Or a b

-- 4 marks
toNNF :: Formula -> NNF
toNNF (Not (Not fr))
  = toNNF fr
toNNF (Not (And fr1 fr2))
  = Or (toNNF (Not fr1)) (toNNF (Not fr2))   
toNNF (Not (Or fr1 fr2))
  = And (toNNF (Not fr1)) (toNNF (Not fr2))   
toNNF (Var nm)
  = Var nm
toNNF (Not (Var nm))
  = Not (Var nm)
toNNF (And fr1 fr2)
  = And (toNNF fr1) (toNNF fr2)
toNNF (Or fr1 fr2)
  = Or (toNNF fr1) (toNNF fr2)

-- 3 marks
toCNF :: Formula -> CNF
toCNF frm
  = toCNF' (toNNF frm)
  where
    toCNF' (And fr1 fr2)
      = And (toCNF' fr1) (toCNF' fr2)
    toCNF' (Or fr1 fr2)
      = distribute (toCNF' fr1) (toCNF' fr2)
    toCNF' fr
      = fr


-- 4 marks
flatten :: CNF -> CNFRep
flatten frm
  = flatten' frm
  where
    idm = idMap frm
    flatten' (Var nm)
      = [[lookUp nm idm]]
    flatten' (Not (Var nm))
      = [[- (lookUp nm idm)]]
    flatten' (Or fr1 fr2)  
      = [(concat.concat) [flatten' fr1, flatten' fr2]]
    flatten' (And fr1 fr2)
      = concat [flatten' fr1, flatten' fr2]

--------------------------------------------------------------------------
-- Part III

-- 5 marks
propUnits :: CNFRep -> (CNFRep, [Int])
propUnits rep
  | nosingletonsLeft rep = (rep, [])
  | otherwise = (newrep, singl : nms)
  where
    singl = (head.head) (singletons rep)
    propUnits' s rep'
      = filter (not.(elem s)) (map (filter (/= (- s))) rep')
    (newrep, nms) = propUnits (propUnits' singl rep)

nosingletonsLeft :: CNFRep -> Bool
nosingletonsLeft 
  = (null.singletons) 

singletons :: CNFRep -> CNFRep
singletons l
  = [k | k <- l, length k == 1]

anyEmpty :: CNFRep -> Bool
anyEmpty l
  = elem [] l

-- 4 marks
dp :: CNFRep -> [[Int]]
dp rep
  | nextRep == []    = [sols]
  | anyEmpty nextRep = []
  | otherwise        = map (sols ++) (dp ([singl] : nextRep)
                       ++ (dp ([- singl] : nextRep)))
  where
      (nextRep, sols) = propUnits rep
      singl = (head.head) nextRep


     

--------------------------------------------------------------------------
-- Part IV

-- Bonus 2 marks
-- Formula == [[Int]]
allSat :: Formula -> [[(Id, Bool)]]
allSat frm
  = concat (map (helper nums []) (((dp.flatten).toCNF) frm))
  where

    rev (x, y) = (y, x) 
    imap = idMap (toCNF frm)
    rimap =  map rev imap
    nums = [num | (num, nm) <- rimap]
    helper :: [Int] -> [(Id, Bool)] -> [Int] -> [[(Id, Bool)]]
    helper [] assigned []
      = [sort (nub assigned)]
    helper (left : lefts) assigned []
      = concat [helper lefts assigned [left], helper lefts assigned [-left]]
    helper lefts assigned (n : ns)
      | n > 0     = helper [q | q <- lefts, q /= n] ((lookUp n rimap, True) : assigned) ns
      | otherwise = helper [q | q <- lefts, q /= (- n)] ((lookUp (- n) rimap, False) : assigned) ns




