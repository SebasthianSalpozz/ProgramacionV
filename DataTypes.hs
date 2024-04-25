
module Data where
import Data.Fixed (Nano)

data NAT = Zero | Suc NAT deriving (Show)

natToNUm :: NAT -> Int
natToNUm Zero = 0
natToNUm (Suc n) = 1 + natToNUm n

numToNat :: Int -> NAT
numToNat 0 = Zero
numToNat n = Suc (numToNat (n-1))

sumaNat :: NAT -> NAT -> NAT
sumaNat m n = numToNat (natToNUm m + natToNUm n)
 
sumaNat2 :: NAT -> NAT -> NAT
sumaNat2 Zero n = n
sumaNat2 (Suc m) n = Suc (sumaNat2 m n)

