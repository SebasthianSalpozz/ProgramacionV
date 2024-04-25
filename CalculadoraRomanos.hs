module Main where
import Distribution.Simple.Command (OptDescr(BoolOpt))
import Text.Read (Lexeme(String))
import Text.Parsec (string)
import Distribution.ModuleName (main)

roman :: [(String,Int)]
roman = [("I",1),("IV", 4),("V",5),("IX", 9),("X",10),("XL", 40),("L",50),("XC", 90),("C",100),("CD", 400),("D",500),("CM", 900),("M",1000)]

expr :: [(String , Int -> Int -> Int )]
expr = [("+", (+)), ("-",(-)), ("*",(*)), ("/", div ), ("%", mod)]

calRoman :: String -> String
calRoman input = result $ words input
        where 
            result :: [String] -> String
            result (x:y:z:xs) = case y of
                                "+" -> numberToRoman ( romanToNumber x + romanToNumber z ) 
                                "-" -> numberToRoman ( romanToNumber x - romanToNumber z )
                                "*" -> numberToRoman ( romanToNumber x * romanToNumber z )
                                ":" -> numberToRoman ( romanToNumber x `div` romanToNumber z )
                                "%"  -> numberToRoman ( romanToNumber x `mod` romanToNumber z )  


romanToNumber :: String -> Int
romanToNumber [] = 0
romanToNumber (x:xs)    | length xs > 0 && findNumber (x:head xs:[])/=[] = 
                                getNumber(x:head xs:[])+ romanToNumber (tail xs) 
                        | otherwise = getNumber[x]+ romanToNumber xs
                        where 
                            getRoman :: String -> Bool
                            getRoman x = findNumber x /= []

                            findNumber:: String-> [(String,Int)]
                            findNumber x = [r |r <- roman, fst r == x ]

getNumber ::String -> Int
getNumber x = snd $ head $ filter (\n -> fst n == x) roman

numberToRoman :: Int -> String
numberToRoman 0 = ""
numberToRoman n = getRoman n ++ numberToRoman (n-getNumber(getRoman n))
                where 
                    getRoman :: Int -> String
                    getRoman n = fst $ head $ filter (\x -> snd x <= n)  (reverse roman)
                    


-- main :: IO ()
-- main = do strings <- getContents
--           mapM_ putStrLn $  map calRoman $ lines strings

main :: IO ()
main = mapM_ putStrLn . map calRoman . lines =<< getContents
