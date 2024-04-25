import Data.Char (digitToInt)

-- numerar los dígitos de derecha a izquierda comenzando desde cero, 
--a cada número se le asigna la correspondiente potencia base 2 y al final se suman las potencias.

binaryToDecimal :: String -> Int
binaryToDecimal binary = sum $ zipWith (\digit power -> digit * 2^power) digits powers
    where
        digits = reverse $ map (\c -> read [c] :: Int) binary
        powers = [0..]

convertTime :: [String]-> String
convertTime [dh, uh, dm, um, ds, us]
    |   binaryToDecimal dh > 2 || binaryToDecimal uh > 9 || 
        binaryToDecimal dm > 6 || binaryToDecimal um > 9 || 
        binaryToDecimal ds > 6 || binaryToDecimal us > 9 = "Error"
    | otherwise =   show (binaryToDecimal dh) ++ show (binaryToDecimal uh) ++ ":" ++ 
                    show (binaryToDecimal dm) ++ show (binaryToDecimal um) ++ ":" ++ 
                    show (binaryToDecimal ds) ++ show (binaryToDecimal us)

inputChange :: [String] -> [String]
inputChange [] = []
inputChange xs = (map head xs) : inputChange (map tail xs)

clock :: [String] -> String
clock = convertTime . inputChange 