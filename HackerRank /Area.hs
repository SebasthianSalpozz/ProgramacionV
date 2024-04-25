import Text.Printf (printf)

-- This function should return a list [area, volume].
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r a b = 
let heights = map fromIntegral [l..r]
    areas = zipWith (*) (map fromIntegral a) heights
    volumes = zipWith (*) areas (map fromIntegral b)
in [sum areas, sum volumes]z