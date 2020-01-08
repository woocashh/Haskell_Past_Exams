

data SuffixTree = Leaf Int | Node [(String, SuffixTree)] 
                deriving (Eq, Show)

------------------------------------------------------

isPrefix :: String -> String -> Bool
isPrefix "" _
  = True
isPrefix (s : ss) ""
  = False
isPrefix (s : ss) (t : ts)
  | s == t    = isPrefix ss ts
  | otherwise = False

removePrefix :: String -> String -> String
removePrefix "" t
--Pre: s is a prefix of s'
  = t
removePrefix (s : ss) (t : ts)
  = removePrefix ss ts

suffixes :: [a] -> [[a]]
suffixes []
  = []
suffixes s
  = s : (suffixes' s)
  where
    suffixes' (t : [])
      = []
    suffixes' (t : ts)
      = ts : (suffixes' ts) 
 
isSubstring :: String -> String -> Bool
isSubstring "" _
  = True
isSubstring (s : ss) ""
  = False
isSubstring s t
  = foldl1 (||) (map (isPrefix s) (suffixes t))

findSubstrings :: String -> String -> [Int]
findSubstrings s t
  = map fst (filter (\(x,y) -> y) (zip [0..] (map (isPrefix s) (suffixes t))))

------------------------------------------------------

getIndices :: SuffixTree -> [Int]
getIndices (Leaf n)
  = [n]
getIndices (Node trees)
  = concat (map (getIndices . snd) trees) 

partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition s t
  = partition' s t []
  where
    partition' a [] l
      = (reverse l, a, [])
    partition' [] b l
      = (reverse l, [], b)
    partition' (a : as) (b : bs) l
      | a == b = partition' as bs (a : l)
      | otherwise = (reverse l, (a : as), (b : bs))

findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' s (Leaf i)
  = []
findSubstrings' s (Node trees)
  =  concat (map helper trees)
  where
    helper (a, tree)
      = helper' (partition s a)
      where
        helper' (pre, su1, su2)
          | su1 == "" = getIndices tree
          | su2 == "" = findSubstrings' su1 tree
          | otherwise = []
  
------------------------------------------------------

getContent (Node listOfPairs) -- Nice helper function!!
  = listOfPairs

insert :: (String, Int) -> SuffixTree -> SuffixTree -- U did not get this
insert (s, n) (Node [])
  = Node [(s, Leaf n)]
insert (s, n) (Node (pair@(a, tree) : pairs))
  | p == a   = Node ((a, insert (r, n) tree) : pairs)
  | null p   = Node (pair : (getContent (insert (r, n) (Node pairs))))
  | p /= a   = Node ([(p, Node [(r, Leaf n), (r', tree)])] ++ pairs)
  where
    (p, r, r') = partition s a


-- This function is given
buildTree :: String -> SuffixTree 
buildTree s
  = foldl (flip insert) (Node []) (zip (suffixes s) [0..])

------------------------------------------------------
-- Part IV

longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring 
  = undefined

-- NEED TO FINISH THIS!!!

------------------------------------------------------
-- Example strings and suffix trees...

s1 :: String
s1 
  = "banana"

s2 :: String
s2 
  = "mississippi"

t1 :: SuffixTree
t1 
  = Node [("banana", Leaf 0), 
          ("a", Node [("na", Node [("na", Leaf 1), 
                                   ("", Leaf 3)]), 
                     ("", Leaf 5)]), 
          ("na", Node [("na", Leaf 2), 
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2 
  = Node [("mississippi", Leaf 0), 
          ("i", Node [("ssi", Node [("ssippi", Leaf 1), 
                                    ("ppi", Leaf 4)]), 
                      ("ppi", Leaf 7), 
                      ("", Leaf 10)]), 
          ("s", Node [("si", Node [("ssippi", Leaf 2), 
                                   ("ppi", Leaf 5)]), 
                      ("i", Node [("ssippi", Leaf 3), 
                                  ("ppi", Leaf 6)])]), 
          ("p", Node [("pi", Leaf 8), 
                      ("i", Leaf 9)])]


