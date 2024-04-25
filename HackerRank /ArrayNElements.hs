import Data.Array

createArray :: Int -> Array Int Int
createArray n = listArray (1, n) [1..n]