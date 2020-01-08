import Data.Maybe
import Data.List

type AttName = String

type AttValue = String

type Attribute = (AttName, [AttValue])

type Header = [Attribute]

type Row = [AttValue]

type DataSet = (Header, [Row])

data DecisionTree = Null |
                    Leaf AttValue | 
                    Node AttName [(AttValue, DecisionTree)]
                  deriving (Eq, Show)

type Partition = [(AttValue, DataSet)]

type AttSelector = DataSet -> Attribute -> Attribute

xlogx :: Double -> Double
xlogx p
  | p <= 1e-100 = 0.0
  | otherwise   = p * log2 p 
  where
    log2 x = log x / log 2

lookUp :: (Eq a, Show a, Show b) => a -> [(a, b)] -> b
lookUp x table
  = fromMaybe (error ("lookUp error - no binding for " ++ show x ++ 
                      " in table: " ++ show table))
              (lookup x table)

--------------------------------------------------------------------
-- PART I
--------------------------------------------------------------------

allSame :: Eq a => [a] -> Bool
allSame []
  = True
allSame l
  = null (tail (nub l)) 

remove :: Eq a => a -> [(a, b)] -> [(a, b)]
remove a l
  = [(x, y) | (x, y) <- l, x /= a]

lookUpAtt :: AttName -> Header -> Row -> AttValue
--Pre: The attribute name is present in the given header.
lookUpAtt attnm h r
  = lookUp attnm (zip (map fst h) r)

removeAtt :: AttName -> Header -> Row -> Row
removeAtt attnm h r
    = map snd (remove attnm (zip (map fst h) r))

addToMapping :: Eq a => (a, b) -> [(a, [b])] -> [(a, [b])]
addToMapping (x, v) []
  = [(x, [v])]
addToMapping (x, v) ((y, vs) : ms)
  | x == y    = (y, v : vs) : ms
  | otherwise = (y, vs) : (addToMapping (x, v) ms) 

buildFrequencyTable :: Attribute -> DataSet -> [(AttValue, Int)]
--Pre: Each row of the data set contains an instance of the attribute
buildFrequencyTable (nm, vals) (h, rs)
  = buildFrequencyTable' rs (zip vals (repeat 0))
  where
    buildFrequencyTable' [] freqTab
      = freqTab
    buildFrequencyTable' (r : rs) freqTab
      = buildFrequencyTable' rs freqTab'
      where
        val = lookUpAtt nm h r  
        freqTab' = (val, (lookUp (val) freqTab) + 1) : (remove val freqTab)

--------------------------------------------------------------------
-- PART II
--------------------------------------------------------------------

nodes :: DecisionTree -> Int
nodes Null
  = 0
nodes (Leaf _)
  = 1
nodes (Node _ ps)
  = 1 + sum (map (nodes.snd) ps) 

evalTree :: DecisionTree -> Header -> Row -> AttValue
evalTree Null _ _
  = ""
evalTree (Leaf res) _ _
  = res
evalTree (Node nm p) h r
  = evalTree (lookUp (lookUpAtt nm h r) p) h r

--------------------------------------------------------------------
-- PART III
--------------------------------------------------------------------

--
-- Given...
-- In this simple case, the attribute selected is the first input attribute 
-- in the header. Note that the classifier attribute may appear in any column,
-- so we must exclude it as a candidate.
--
nextAtt :: AttSelector
--Pre: The header contains at least one input attribute
nextAtt (header, _) (classifierName, _)
  = head (filter ((/= classifierName) . fst) header)

partitionData :: DataSet -> Attribute -> Partition
partitionData (h, rws) (nm, vals)
  = zip (fst res) (map helper (snd res))
  where
    res = unzip (partitionData' rws (zip vals (repeat [])))
    helper l = (h', l)
    h' = remove nm h

    partitionData' [] part
      = part
    partitionData' (r : rs) part
      = partitionData' rs part'
      where 
        r' = removeAtt nm h r
        val = lookUpAtt nm h r
        part' = addToMapping (val, r') part

buildTree :: DataSet -> Attribute -> AttSelector -> DecisionTree 
buildTree (_, []) _ _
  = Null
buildTree (h, rs) (clnm, clvals) sel
  | allSame (map (lookUpAtt clnm h) rs) = Leaf (lookUpAtt clnm h (head rs)) 
  | otherwise                           = Node nm' (zip vals' (map helper datasets))
  where
    (nm', vals') = sel (h, rs) (clnm, clvals)
    (values, datasets) = unzip (partitionData (h, rs) (nm', vals'))
    helper ds = buildTree ds (clnm, clvals) sel

--------------------------------------------------------------------
-- PART IV
--------------------------------------------------------------------

prob :: DataSet -> Attribute -> AttValue -> Double
prob 
  = undefined

entropy :: DataSet -> Attribute -> Double
entropy (h, []) attr = 0
entropy ds@(h, rs) attr 
  = - sum (map (calc2.calc1) (buildFrequencyTable attr ds))
  where
    calc2 = xlogx.(/ d)
    calc1 = fromIntegral.snd
    d = (fromIntegral.length) rs

gain :: DataSet -> Attribute -> Attribute -> Double
gain ds@(h, rs) attr clattr
  = (entropy ds clattr) - (sum (zipWith (*) ps parts))
  where
    ps = map (((/ d).fromIntegral).snd) (buildFrequencyTable attr ds)
    d = (fromIntegral.length) rs
    parts = map (helper.snd) (partitionData ds attr)
    helper x = entropy x clattr 

bestGainAtt :: AttSelector
bestGainAtt ds@(h, rs) clattr@(nm, val)
  = ((snd.head).reverse) (sort (zip (map helper h') h'))
  where
    helper attr = gain ds attr clattr
    h' = remove nm h

--------------------------------------------------------------------

outlook :: Attribute
outlook 
  = ("outlook", ["sunny", "overcast", "rainy"])

temp :: Attribute 
temp 
  = ("temp", ["hot", "mild", "cool"])

humidity :: Attribute 
humidity 
  = ("humidity", ["high", "normal"])

wind :: Attribute 
wind 
  = ("wind", ["windy", "calm"])

result :: Attribute 
result
  = ("result", ["good", "bad"])

fishingData :: DataSet
fishingData
  = (header, table)

header :: Header
table  :: [Row]
header 
  =  [outlook,    temp,   humidity, wind,    result] 
table 
  = [["sunny",    "hot",  "high",   "calm",  "bad" ],
     ["sunny",    "hot",  "high",   "windy", "bad" ],
     ["overcast", "hot",  "high",   "calm",  "good"],
     ["rainy",    "mild", "high",   "calm",  "good"],
     ["rainy",    "cool", "normal", "calm",  "good"],
     ["rainy",    "cool", "normal", "windy", "bad" ],
     ["overcast", "cool", "normal", "windy", "good"],
     ["sunny",    "mild", "high",   "calm",  "bad" ],
     ["sunny",    "cool", "normal", "calm",  "good"],
     ["rainy",    "mild", "normal", "calm",  "good"],
     ["sunny",    "mild", "normal", "windy", "good"],
     ["overcast", "mild", "high",   "windy", "good"],
     ["overcast", "hot",  "normal", "calm",  "good"],
     ["rainy",    "mild", "high",   "windy", "bad" ]]

--
-- This is the same as the above table, but with the result in the second 
-- column...
--
fishingData' :: DataSet
fishingData'
  = (header', table')

header' :: Header
table'  :: [Row]
header' 
  =  [outlook,    result, temp,   humidity, wind] 
table' 
  = [["sunny",    "bad",  "hot",  "high",   "calm"],
     ["sunny",    "bad",  "hot",  "high",   "windy"],
     ["overcast", "good", "hot",  "high",   "calm"],
     ["rainy",    "good", "mild", "high",   "calm"],
     ["rainy",    "good", "cool", "normal", "calm"],
     ["rainy",    "bad",  "cool", "normal", "windy"],
     ["overcast", "good", "cool", "normal", "windy"],
     ["sunny",    "bad",  "mild", "high",   "calm"],
     ["sunny",    "good", "cool", "normal", "calm"],
     ["rainy",    "good", "mild", "normal", "calm"],
     ["sunny",    "good", "mild", "normal", "windy"],
     ["overcast", "good", "mild", "high",   "windy"],
     ["overcast", "good", "hot",  "normal", "calm"],
     ["rainy",    "bad",  "mild", "high",   "windy"]]

fig1 :: DecisionTree
fig1
  = Node "outlook" 
         [("sunny", Node "temp" 
                         [("hot", Leaf "bad"),
                          ("mild",Node "humidity" 
                                       [("high",   Leaf "bad"),
                                        ("normal", Leaf "good")]),
                          ("cool", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "temp" 
                         [("hot", Null),
                          ("mild", Node "humidity" 
                                        [("high",Node "wind" 
                                                      [("windy",  Leaf "bad"),
                                                       ("calm", Leaf "good")]),
                                         ("normal", Leaf "good")]),
                          ("cool", Node "humidity" 
                                        [("high", Null),
                                         ("normal", Node "wind" 
                                                         [("windy",  Leaf "bad"),
                                                          ("calm", Leaf "good")])])])]

fig2 :: DecisionTree
fig2
  = Node "outlook" 
         [("sunny", Node "humidity" 
                         [("high", Leaf "bad"),
                          ("normal", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "wind" 
                         [("windy", Leaf "bad"),
                          ("calm", Leaf "good")])]


outlookPartition :: Partition
outlookPartition
  = [("sunny",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","bad"],["hot","high","windy","bad"],
                   ["mild","high","calm","bad"],["cool","normal","calm","good"],
                   ["mild","normal","windy","good"]])),
     ("overcast",([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","good"],["cool","normal","windy","good"],
                   ["mild","high","windy","good"],["hot","normal","calm","good"]])),
     ("rainy",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["mild","high","calm","good"],["cool","normal","calm","good"],
                   ["cool","normal","windy","bad"],["mild","normal","calm","good"],
                   ["mild","high","windy","bad"]]))]