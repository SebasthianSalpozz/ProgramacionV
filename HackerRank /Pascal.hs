import Data.List (genericIndex)

pascalTriangle :: Int -> [[Int]]
pascalTriangle n = take n triangle
    where
        triangle = iterate nextRow [1]
        nextRow row = zipWith (+) (0:row) (row ++ [0])

printPascal :: Int -> IO ()
printPascal n = mapM_ printRow (pascalTriangle n)
    where
        printRow row = putStrLn $ unwords $ map show row

