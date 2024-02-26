--Task of Sebasthian Salinas

esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo 2 = True
esPrimo n = if (length [x | x <- [2..n-1], mod n x == 0]) > 0 then False else True

numPrimos :: Int -> [Int]
numPrimos n = filter esPrimo [1..n]