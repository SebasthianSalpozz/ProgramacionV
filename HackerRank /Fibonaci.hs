fibonacci :: Int -> [Int]
fibonacci n = take n fibSeq
    where
        fibSeq = 0 : 1 : zipWith (+) fibSeq (tail fibSeq)