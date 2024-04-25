module Scanner where

type Col = Int
type Line = Int
type Val = String

data Token = Token Type String Val Line Col  
        deriving (Eq)
data Type = String
            |OpenBlock 
            |EndBlock
            |Keyword
            |Error
        deriving (Show, Eq)
type Input= String

scanner :: String -> [Token]
scanner xs = scan xs 1 1

scan :: String -> Line -> Col -> [Token]
scan [] _ _ = []
scan (x:xs) l c 
    |x == '!' = Token Keyword [x] l c:scan xs l (c+1)
    |x == '{' = Token OpenBlock [x] l c:scan xs l (c+1)
