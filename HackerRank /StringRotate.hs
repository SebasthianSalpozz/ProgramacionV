rotateString :: String -> [String]
rotateString s = take (length s) (iterate (\x -> tail x ++ [head x]) s) 