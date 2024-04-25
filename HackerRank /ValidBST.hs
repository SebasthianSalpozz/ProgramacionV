isPreorderBST :: [Int] -> Bool
isPreorderBST [] = True
isPreorderBST [x] = True
isPreorderBST (x:xs) =
    let (left, right) = span (< x) xs
        right' = dropWhile (< x) right
    in all (> x) right' && isPreorderBST left && isPreorderBST right'