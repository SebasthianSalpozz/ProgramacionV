
histograma:: [Int] -> String
histograma []= []
histograma xs = histoString (histoList xs) 

histoList :: [Int] -> [String]
histoList xs = [replicate n '*' | n <- conteo xs ]

conteo :: [Int] -> [Int]
conteo xs = [length (filter (==x) xs) | x <- [0..9]]

histoString :: [String] -> String
histoString [] = []
histoString (x:xs) = show x ++ x ++ histoString xs

cadena = "12\n12\n"
--putString("\n")
 