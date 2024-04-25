module Expresions where
    data Exp = Lit Int
                | Add Exp Exp
                | Mul Exp Exp
                | If Cond Exp Exp
                deriving Eq
    data Cond = Eq Exp Exp
                | Lt Exp Exp
                | Gt Exp Exp
                | Dif Exp Exp
                deriving Eq

    instance Show Exp where
        show (Lit n) = show n
        show (Add a b)=  par (show a) ++ "+" ++ par (show b)
        show (Mul a b)= par (show a) ++ "*" ++ par (show b)
        show (If c a b) = "if " ++ show c ++ " then " ++ show a ++ " else " ++ show b
    
    instance Show Cond where
        show (Eq a b) = show a ++ "==" ++ show b
        show (Lt a b) = show a ++ "<" ++ show b
        show (Gt a b) = show a ++ ">" ++ show b
        show (Dif a b) = show a ++ "!=" ++ show b 

    evelExp :: Exp -> Int
    evelExp (Lit n) = n
    evelExp (Add a b) = evelExp a + evelExp b
    evelExp (Mul a b) = evelExp a * evelExp b
    evelExp (If c a b) = if evelCond c then evelExp a else evelExp b

    evelCond :: Cond -> Bool
    evelCond (Eq a b) = evelExp a == evelExp b
    evelCond (Lt a b) = evelExp a < evelExp b
    evelCond (Gt a b) = evelExp a > evelExp b
    evelCond (Dif a b) = evelExp a /= evelExp b

    par :: String -> String
    par x = "(" ++ x ++ ")"

    example0 = Add (Lit 1) (Mul (Lit 2) (Lit 3))
    e1 = Mul(Add(Lit 1)(Lit 2))(Lit 3)
    e2 = Add example0 (Mul (Lit 3) e1)

    c0 = Lt example0 e2
    e3 = If c0 example0 e2
    e4 = Dif e3 e2