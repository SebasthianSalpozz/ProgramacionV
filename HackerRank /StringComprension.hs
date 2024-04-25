compress :: String -> String
compress [] = []
compress (x:xs) = contador 1 x xs
    where
        contador :: Int -> Char -> String -> String
        contador count caracter [] = if count > 1 then caracter : show count else [caracter]
        contador count caracter (y:ys)
            | caracter == y = contador (count + 1) caracter ys
            | count > 1 = caracter : show count ++ contador 1 y ys
            | otherwise = caracter : contador 1 y ys