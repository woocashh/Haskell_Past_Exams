import Data.Maybe
import Data.List
import Data.Char

toNum :: Maybe a -> Int
toNum 
  = maybe 0 (const 1) 

filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe p (Just x)
  | p x = Just x
  | otherwise = Nothing
filterMaybe p _
  = Nothing


data Shape = Triangle Float Float Float | Square Float | Circle Float 
  | Polygon [(Float, Float)]

area :: Shape -> Float
area (Triangle a b c)
  = triangleArea a b c
area (Square d)
  = d * d
area (Circle r)
  = pi * r ^ 2
area (Polygon  (c1 : c2 : c3 : coords))
  = (triangleArea (lineLen c1 c2) (lineLen c1 c3) (lineLen c2 c3)) + 
    area (Polygon (c2 : c3 : coords))
area (Polygon _)
    = 0

triangleArea a b c
  = sqrt (s * (s - a) * (s - b) * (s - c))
  where
    s = (a + b + c) / 2

lineLen (x1, y1) (x2, y2)
  = sqrt ((x2 - x1) ^ 2 + (y2 - y1)^2) 

data Tree = Leaf | Node Tree Tree
  deriving (Eq, Show)

makeTrees :: Int -> [Tree]
makeTrees 0
  = [Leaf]
makeTrees n
  = [Node t1 t2 | k <- [0..(n - 1)], l <- [0..(n - 1)], 
    t1 <- makeTrees (k), t2 <- makeTrees (l), (l + k) == (n - 1)]


data Colour = Red | Green | Blue
            deriving (Show, Bounded, Enum)

data AmPm  = AM | PM
           deriving (Show, Eq)
        
data Time  = TwoFour Int Int | Clock Int Int AmPm


to24 :: Time -> Time
to24 (TwoFour h min)
  = TwoFour h min
to24 (Clock 12 min AM)
  = TwoFour 0 min
to24 (Clock h min AM)
  = TwoFour h min
to24 (Clock 12 min PM)
  = TwoFour 12 min  
to24 (Clock h min PM)
  = TwoFour (h+12) min

equalTime :: Time -> Time -> Bool
equalTime t1 t2
  = isSame (to24 t1) (to24 t2)
  where 
    isSame (TwoFour h m) (TwoFour h' m')
      = (h == h' && m == m')

instance Eq Time where
  (==) = equalTime

instance Show Time where
  show (TwoFour h m) = show' h ++ " : " ++ show' m ++" HRS"
  where 
    show' n
      | n < 10 = "0" ++ show n
      | otherwise = show n



