toDigits :: Int -> [Int]
toDigits n  | n <= 0=[]
            | otherwise=map(\x -> read [x])(show n)

toDigitsRev :: Int -> [Int]
toDigitsRev n = reverse (toDigits n)

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:xs) = x : (2 * y) : doubleEveryOther xs

sumDigits :: [Int] -> Int
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

validate :: Int -> Bool
validate n | sumDigits(doubleEveryOther(toDigitsRev n)) `mod` 10 == 0 = True
           | otherwise = False