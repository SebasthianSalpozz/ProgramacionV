replicateList :: Int -> [Int] -> [Int]
replicateList n xs = concatMap (replicate n) xs