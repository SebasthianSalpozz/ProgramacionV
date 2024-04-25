data BinaryTree a = Node a (BinaryTree a) (BinaryTree a) | Empty deriving (Show)

createTree :: Int -> [[Int]] -> BinaryTree Int
createTree profundidad arr = head subtrees
  where subtrees = [getSubtree x | x <- [0..profundidad - 1]]
        getSubtree x = (\[leftChild, rightChild] -> Node (x + 1) leftChild rightChild) . map (\x -> if x == -1 then Empty else subtrees !! (x - 1)) $ arr !! x

swapTree :: Int -> BinaryTree Int -> BinaryTree Int
swapTree nivel = swap 1
  where swap _ Empty = Empty
        swap altura (Node x l r)
          | altura `rem` nivel == 0 = Node x (swap (altura + 1) r) (swap (altura + 1) l)
          | otherwise = Node x (swap (altura + 1) l) (swap (altura + 1) r)
inorder :: BinaryTree Int -> [Int]
inorder Empty = []
inorder (Node x left right) = inorder left ++ [x] ++ inorder right

solve :: [Int] -> BinaryTree Int -> IO ()
solve [] tree = return ()
solve (levelList:ks) tree = putStrLn (unwords . map show . inorder $ ans) >> solve ks ans
  where ans = swapTree levelList tree


validate :: [[Int]] -> IO ()
validate ([n]:rest) = solve k tree
  where (arr, t:k) = (\(f, s) -> (f, concat s)) . splitAt n $ rest
        tree = createTree n arr

main :: IO ()
main = getContents >>= validate . map (map read . words) . lines
