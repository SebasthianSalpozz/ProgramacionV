{-# LANGUAGE UndecidableInstances #-}


module Scanner where 

import Data.Char (isAlphaNum)

-- cabal install uulib

type Col = Int
type Line = Int 
type Value = String
type Input = String

data Token = Token Type Value Line Col

data Type = String 
          | OpenBlock
          | EndBlock
          | Keyword
          | EndSlide
          | Error
          | Comment
          | OpenCode
          | CloseCode
        deriving(Eq, Ord)

instance Show Token where
    show (Token t v l c) = show t ++ show v ++ " " ++ show l ++ " " ++ show c ++ "\n"

instance Show Type where
    show String = "String: "
    show OpenBlock = "OpenBlock: "
    show EndBlock = "EndBlock: "
    show Keyword = "Keyword: "
    show Error = "Error: "
    show EndSlide = "EndSlide: "
    show OpenCode = "OpenCode: "
    show CloseCode = "CloseCode: "
    -- show Comment = "Comment: "
instance (Eq Type) => (Eq Token) where
    (Token String _ _ _) == (Token String _ _ _) = True
    (Token OpenBlock _ _ _) == (Token OpenBlock _ _ _) = True
    (Token EndBlock _ _ _) == (Token EndBlock _ _ _) = True
    (Token Keyword k1 _ _) == (Token Keyword k2 _ _) = k1 == k2
    (Token Error k1 _ _) == (Token Error k2 _ _) = k1 == k2
    (Token EndSlide _ _ _) == (Token EndSlide _ _ _) = True
    (Token OpenCode s1 _ _) == (Token OpenCode s2 _ _) = s1==s2
    (Token CloseCode s1 _ _) == (Token CloseCode s2 _ _) = s1==s2
    (Token t1 s1 _ _ ) == (Token t2 s2 _ _ ) = t1 == t2 && s1 == s2
    
    
instance Ord Token where
    compare x y | x == y = EQ
                | x <= y = LT
                | otherwise = GT
    (Token t1 s1 _ _ ) <= (Token t2 s2 _ _ ) = t1 < t2 || (t1 == t2 && s1 <= s2)

scanner :: Input -> [Token]
scanner xs = scan xs 1 1

scan :: Input -> Line -> Col -> [Token]
scan [] _ _ = []
scan (x:xs) l c 
  | x == '!' = Token Keyword [x] l c: scan xs l (c+1) 
  | x == '·' = Token Keyword [x] l c: scan xs l (c+1)   
  | x == ' ' = scan xs l (c+1)
  | x == '\n' = scan xs (l+1) 1
  | x == '{' = Token OpenBlock [x] l c: scan xs l (c+1)
  | x == '}' = Token EndBlock [x] l c: scan xs l (c+1)
  | x == ';' = scan (dropWhile (/= '\n') xs) (l+1) 1
  | x == '-' && xs /= [] && head xs == '-' && tail xs /= [] && head (tail xs) == '-' = Token EndSlide [x, head xs, head (tail xs)] l c: scan (drop 2 xs) l (c+3)
  | isAlphaNum x || x `elem` " :/?&=.-_~" = let (token, rest) = span (\c -> isAlphaNum c || c `elem` " :/?&=.-_~") (x:xs)
                                                                                                                                                                                                                     in Token String token l c : scan rest l (c + length token)
  | x == '#' = let (token, rest) = span (\c -> c == '#') (x:xs)
                         in Token Keyword token l c : scan rest l (c + length token) 
  | x == '*' = let (token, rest) = span (\c -> c == '*') (x:xs)
                         in Token Keyword token l c : scan rest l (c + length token) 
  | x == '$' = let (token, rest) = span (\c -> c == '$') (x:xs)
                         in Token Keyword token l c : scan rest l (c + length token)
  | x == '+' = 
                let (listItem, rest) = span (== '+') (x:xs)
                    tokenLength = length listItem
                in Token Keyword listItem l c : scan rest l (c + tokenLength)
  | x == '<' = Token OpenCode [x] l c: scan xs l (c+1)
  | x == '>' = Token CloseCode [x] l c: scan xs l (c+1)
  | otherwise = Token Error [x] l c: scan xs l (c+1)
