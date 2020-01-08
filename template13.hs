import Data.List (reverse)
type BinHeap a = [BinTree a]

data BinTree a = Node a Int (BinHeap a)
               deriving (Eq, Ord, Show)

--------------------------------------------------------------
-- PART I

value :: BinTree a -> a
value (Node val _ _)
  = val

rank :: BinTree a -> Int
rank (Node _ rnk _)
  = rnk

children :: BinTree a -> [BinTree a]
children (Node _ _ chs)
  = chs

combineTrees :: Ord a => BinTree a -> BinTree a -> BinTree a
combineTrees bt1 bt2
  | v1 <= v2  = (Node v1 ((rank bt1) + 1) (bt2 : (children bt1)))
  | otherwise = (Node v2 ((rank bt2) + 1) (bt1 : (children bt2)))
  where
      v1 = value bt1
      v2 = value bt2

--------------------------------------------------------------
-- PART II

extractMin :: Ord a => BinHeap a -> a
extractMin lbhs
  = minimum ( map value lbhs)

mergeHeaps :: Ord a => BinHeap a -> BinHeap a -> BinHeap a
mergeHeaps [] bh2
  = bh2
mergeHeaps bh1 []
  = bh1
mergeHeaps h@(t : bh1) h'@(t' : bh2)
  | r < r'  = (t : (mergeHeaps bh1 h'))
  | r > r'  = (t' : (mergeHeaps h bh2))
  | r == r' = mergeHeaps [combineTrees t t'] (mergeHeaps bh1 bh2)
  where
      r = rank t
      r' = rank t'

insert :: Ord a => a -> BinHeap a -> BinHeap a
insert val bh
  = mergeHeaps [Node val 0 []] bh

deleteMin :: Ord a => BinHeap a -> BinHeap a
deleteMin bh
  = mergeHeaps (reverse (childrenTree mini bh)) (remove mini bh)
  where
      mini = extractMin bh

remove :: Ord a => a -> BinHeap a -> BinHeap a
remove val []
  = []
remove val (t : bh)
  | val == value t = bh
  | otherwise = t : (remove val bh)

childrenTree _ []
  = []
childrenTree val (t : ts)
  | val == value t = children t
  | otherwise = childrenTree val ts

removeMin :: Ord a => BinHeap a -> (BinTree a, BinHeap a)
removeMin
  = undefined

binSort :: Ord a => [a] -> [a]
binSort []
  = []
binSort a
  = helper (binSort' a [])
  where 
    binSort' [] heap
      = heap
    binSort' (x : xs) heap
      = binSort' xs (insert x heap)

    helper []
      = []
    helper h
      = (extractMin h) : (helper (deleteMin h))
    
--------------------------------------------------------------
-- PART III

toBinary :: BinHeap a -> [Int]
toBinary b
  = toBinary' (rank (head rb)) rb
  where
    rb = reverse b
    toBinary' (-1) []
      = []
    toBinary' r []
      = (0 : (toBinary' (r - 1) []))
    toBinary' r (t : ts)
      | (rank t) == r = (1 : toBinary' (r - 1) ts)
      | otherwise     = (0 : toBinary' (r - 1) (t : ts)) 
      
binarySum :: [Int] -> [Int] -> [Int]
binarySum
  = undefined

------------------------------------------------------
-- Some sample trees...

t1, t2, t3, t4, t5, t6, t7, t8 :: BinTree Int
-- Note: t7 is the result of merging t5 and t6

-- t1 to t4 appear in Figure 1...
t1 = Node 4 0 []
t2 = Node 1 1 [Node 5 0 []]
t3 = Node 2 2 [Node 8 1 [Node 9 0 []], 
               Node 7 0 []]
t4 = Node 2 3 [Node 3 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []],
               Node 8 1 [Node 9 0 []],
               Node 7 0 []]

-- t5 and t6 are on the left of Figure 2; t7 is on the
-- right
t5 = Node 4 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []]
t6 = Node 2 2 [Node 8 1 [Node 9 0 []], Node 7 0 []]
t7 = Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []], Node 10 0 []],
               Node 8 1 [Node 9 0 []], 
               Node 7 0 []]

-- An additional tree...
t8 = Node 12 1 [Node 16 0 []]

------------------------------------------------------
-- Some sample heaps...

h1, h2, h3, h4, h5, h6, h7 :: BinHeap Int
-- Two arbitrary heaps for testing...
h1 = [t2, t7]
h2 = [Node 1 2 [Node 12 1 [Node 16 0 []],
                Node 5 0 []],
      Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 8 1 [Node 9 0 []],
                Node 7 0 []]]

-- h3 is shown in Figure 3...
h3 = [t1, t2, t4]

-- Two additional heaps, used below. They are shown
-- in Figure 4(a)...

h4 = [t2, t5]
h5 = [t1, t8]

-- h6 is the result of merging h4 and h5, shown in Figure 4(b)...
h6 = [Node 4 0 [],
      Node 1 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 12 1 [Node 16 0 []],
                Node 5 0 []]]

-- h7 is shown in Figure 5...
h7 = [Node 4 3 [Node 4 2 [Node 12 1 [Node 16 0 []],
                          Node 5 0 []],
                Node 6 1 [Node 8 0 []],
                Node 10 0 []]]



