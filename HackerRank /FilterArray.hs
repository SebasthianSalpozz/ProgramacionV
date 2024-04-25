filterArray :: Int -> [Int] -> [Int]
filterArray limit = filter (< limit)

main :: IO ()
main = do
    limit <- readLn :: IO Int
    numbers <- fmap read . lines <$> getContents
    let filteredNumbers = filterArray limit numbers
    mapM_ print filteredNumbers