import Data.List
import Data.Maybe

type Index = Int

data BExp = Prim Bool | IdRef Index | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)

type Env = [(Index, Bool)]

type NodeId = Int

type BDDNode =  (NodeId, (Index, NodeId, NodeId))

type BDD = (NodeId, [BDDNode])

------------------------------------------------------
-- PART I

-- Pre: The item is in the given table
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp 
  = ((.).(.)) fromJust lookup 

checkSat :: BDD -> Env -> Bool
checkSat (r, []) _
  | r == 1 = True
  | r == 0 = False
checkSat (r,ns) env
  = checkSat' (lookUp r ns)
  where
    checkSat' (i, f, t) 
      | lookUp i env = helper t
      | otherwise    = helper f
      where
        helper q
          | q == 1    = True
          | q == 0    = False
          | otherwise = checkSat' (lookUp q ns)

sat :: BDD -> [[(Index, Bool)]]
sat (r, ns)
  | r == 0 = []
  | r == 1 = [[]]
  | otherwise = sat' (lookUp r ns) []
  where 
    sat' (i, f, t) acc
      | t == 1 && f == 1 = [(i, True) : acc, (i,False) : acc]
      | t == 1 && f == 0 = [(i,True) : acc]
      | t == 0 && f == 1 = [(i,False) : acc]
      | t == 0 && f == 0 = []
      | otherwise        = (sat' (lookUp f ns) ((i, False) : acc)) 
                            ++ (sat' (lookUp t ns) ((i,True) : acc)) 

------------------------------------------------------
-- PART II

simplify :: BExp -> BExp
simplify (Not (Prim q))
  = Prim (not q)
simplify (And (Prim q) (Prim p)) 
  = Prim (q && p)
simplify (Or (Prim q) (Prim p))
  = Prim (q || p)
simplify b
  = b

restrict :: BExp -> Index -> Bool -> BExp
restrict (IdRef n) i p
  | n == i    = Prim p
  | otherwise = IdRef n
restrict (Prim q) i p
  = Prim q
restrict (Not a) i p
  = simplify (Not (restrict a i p))
restrict (And a b) i p
  = simplify (And (restrict a i p) (restrict b i p))
restrict (Or a b) i p
  = simplify (Or (restrict a i p) (restrict b i p))   

------------------------------------------------------
-- PART III

-- Pre: Each variable index in the BExp appears exactly once
--     in the Index list; there are no other elements
-- The question suggests the following definition (in terms of buildBDD')
-- but you are free to implement the function differently if you wish.
buildBDD :: BExp -> [Index] -> BDD
buildBDD (Prim True) _
  = (1,[])
buildBDD (Prim False) _
  = (0,[])
buildBDD e xs
  = (2, buildBDD' e 2 xs)

-- Potential helper function for buildBDD which you are free
-- to define/modify/ignore/delete/embed as you see fit.
buildBDD' :: BExp -> NodeId -> [Index] -> [BDDNode]
buildBDD' e nid (x : [])
  = [(nid, (x, helper (restrict e x False), helper (restrict e x True)))]
  where
    helper :: BExp -> NodeId
    helper (Prim True)  = 1
    helper (Prim False) = 0
buildBDD' e nid (x : xs)
  = (nid,(x, 2 * nid, 2 * nid + 1)) 
      : ((buildBDD' (restrict e x False) (2 * nid) xs) 
      ++ (buildBDD' (restrict e x True) (2 * nid + 1) xs))

------------------------------------------------------
-- PART IV

-- Pre: Each variable index in the BExp appears exactly once
--      in the Index list; there are no other elements
buildROBDD :: BExp -> [Index] -> BDD
buildROBDD e xs
  = opt (buildBDD e xs)

opt :: BDD -> BDD
opt (r, ns)
  = (r, opt' ns ns)
  where
    opt' [] ms'
      = ms'
    opt' ((nid, (x, f, t)) : ms) ms'
      | f == t    = opt' ms' (helper nid ms')
      | otherwise = opt' ms ms'
      where
        helper _ []
          = []
        helper nid' ((n, (x', f' , t' )) : ms'')
          | nid' == n         = helper nid' ms''
          | ((div nid' 2) == n) && (mod nid' 2 == 0) = (n,(x', f, t')) : (helper nid' ms'')  
          | ((div nid' 2) == n) && (mod nid' 2 == 1) = (n,(x', f', t)) : (helper nid' ms'') 
          | otherwise         = (n, (x', f' , t' )) : helper nid' ms''  





------------------------------------------------------
-- Examples for testing...

b1, b2, b3, b4, b5, b6, b7, b8 :: BExp
b1 = Prim False
b2 = Not (And (IdRef 1) (Or (Prim False) (IdRef 2)))
b3 = And (IdRef 1) (Prim True)
b4 = And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3)))
b5 = Not (And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3))))
b6 = Or (And (IdRef 1) (IdRef 2)) (And (IdRef 3) (IdRef 4))
b7 = Or (Not (IdRef 3)) (Or (IdRef 2) (Not (IdRef 9)))
b8 = Or (IdRef 1) (Not (IdRef 1))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(4,(2,1,1)),(5,(2,1,0)),(2,(1,4,5))])
bdd3 = (5,[(5,(1,0,1))])
bdd4 = (2,[(2,(2,4,5)),(4,(3,8,9)),(8,(7,0,1)),(9,(7,0,0)),
           (5,(3,10,11)),(10,(7,0,1)),(11,(7,0,1))])
bdd5 = (3,[(4,(3,8,9)),(3,(2,4,5)),(8,(7,1,0)),(9,(7,1,1)),
           (5,(3,10,11)),(10,(7,1,0)),(11,(7,1,0))])
bdd6 = (2,[(2,(1,4,5)),(4,(2,8,9)),(8,(3,16,17)),(16,(4,0,0)),
           (17,(4,0,1)),(9,(3,18,19)),(18,(4,0,0)),(19,(4,0,1)),
           (5,(2,10,11)),(10,(3,20,21)),(20,(4,0,0)),(21,(4,0,1)),
           (11,(3,22,23)),(22,(4,1,1)),(23,(4,1,1))])
bdd7 = (6,[(6,(2,4,5)),(4,(3,8,9)),(8,(9,1,1)),(9,(9,1,0)),
           (5,(3,10,11)),(10,(9,1,1)),(11,(9,1,1))])
bdd8 = (2,[(2,(1,1,1))])


