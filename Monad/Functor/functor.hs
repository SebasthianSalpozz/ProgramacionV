-- Que es un Functor
--Un functor es una clase de tipo que define la función fmap que permite aplicar una función a los valores contenidos en el functor, preservando la estructura subyacente. 
--Esto permite transformar los valores dentro del functor sin modificar su contexto.

-- Functor Laws
-- Los functors deben satisfacer dos leyes ecuacionales:
-- Ley de identidad: 
-- fmap id = id 
-- significa que aplicar fmap con la función identidad (id) a un functor no debe cambiar su valor.

-- Ley de composición: 
-- fmap (f . g) = fmap f . fmap g 
-- significa que aplicar fmap a la composición de dos funciones (f y g) es equivalente a aplicar fmap a cada función por separado y luego componer los resultados.

import Text.Parsec

data Expr t = Add (Expr t) (Expr t)
            | Sub (Expr t) (Expr t)
            | Mult (Expr t) (Expr t)
            | Div (Expr t) (Expr t)
            | Neg (Expr t)
            | Pos (Expr t)
            | Lit t
            deriving (Eq, Show)
  
  
exponenciacion :: (Integral t) => t -> t -> t -> t
exponenciacion m base e
    | e == 0 = 1
    | e == 1 = base `mod` m
    | e `mod` 2 == 0 = (ediv2 * ediv2) `mod` m
    | otherwise  = (((ediv2 * ediv2) `mod` m) * base) `mod` m
    where
        ediv2 = exponenciacion m base (e `div` 2)
        
evalExp :: Int -> Expr Int -> Int
evalExp m (Add l r) = ((evalExp m l) + (evalExp m r)) `mod` m
evalExp m (Sub l r) = ((evalExp m l) - (evalExp m r)) `mod` m
evalExp m (Mult l r) = ((evalExp m l) * (evalExp m r)) `mod` m
evalExp m (Div l r) = ((evalExp m l) * (exponenciacion m (evalExp m r) (m-2))) `mod` m
evalExp m (Neg c) = -(evalExp m c)
evalExp m (Pos c) = evalExp m c
evalExp m (Lit c) = c `mod` m

literal :: (Read t, Num t) => Parsec String u (Expr t)
literal = fmap (Lit . read) $ many1 digit


factor =   literal
       <|> fmap Neg (char '-' >> spaces >> factor)
       <|> fmap Pos (char '+' >> spaces >> factor)
       <|> do
            char '('
            spaces
            expr <- expression
            spaces
            char ')'
            return expr
            
term = do
    f <- factor
    rest <- optionMaybe $ try $ do
        spaces
        op <- oneOf "*/"
        spaces
        t <- term
        case op of
            '*' -> return (Mult f t)
            '/' -> return (Div f t)
    case rest of
        Nothing -> return f
        Just ft -> return ft
            
expression = do
    t <- term
    rest <- optionMaybe $ try $ do
        spaces
        op <- oneOf "+-"
        spaces
        expr <- expression
        case op of
            '+' -> return (Add t expr)
            '-' -> return (Sub t expr)
    case rest of
        Nothing -> return t
        (Just te) -> return te
        
main = do
    let m = 1000000007 :: Int
    str <- getLine
    let expr = (parse expression "" str)::(Either ParseError (Expr Int))
        val = fmap (evalExp m) expr
        ans (Right s) = s
    print $ ans val

