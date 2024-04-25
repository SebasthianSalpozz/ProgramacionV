removeOddPositions :: [a] -> [a]
removeOddPositions = map snd . filter (even . fst) . zip [1..]

main :: IO ()
main = do
    input <- getContents
    let numbers = map read (lines input) :: [Int]
            let result = removeOddPositions numbers
    mapM_ print result