import Data.List (sort, (\\))

findMissingNumbers :: [Int] -> [Int] -> [Int]
findMissingNumbers a b = sort (b \\ a)