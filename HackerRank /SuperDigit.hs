superDigit :: Int -> Int -> Int
superDigit n k
    | n < 10    = n
    | otherwise = superDigit (sumDigits (n * k)) 1

sumDigits :: Int -> Int
sumDigits n
    | n < 10    = n
    | otherwise = (n `mod` 10) + sumDigits (n `div` 10)