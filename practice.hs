import Data.Bits

showBitVector :: Int -> Int -> String
showBitVector bv 0
  = ""
showBitVector bv n
  = showBitVector (bv `div` 2) (n - 1) ++ show (bv `mod` 2)

infixl 5 .@.
x .@. y = ((complement x) .&. y) .|. ((complement y) .&. x)

nand x y = complement (x .&. y)

createMask :: Int -> Int
createMask x = bit x -1

findTheNum :: [Int] -> Int
findTheNum l = foldl xor 0 l

isOdd :: Int -> Bool
isOdd x = testBit (x .&. 1) 0 


adder :: Int -> Int -> Int
adder x y
  | y == 0 = x
  | otherwise = adder (xor x y) (shiftL (x .&. y) 1)

createMovingMask :: Int -> Int -> Int -> Int
createMovingMask x s len 
  = shiftR x s .&. (bit len - 1)