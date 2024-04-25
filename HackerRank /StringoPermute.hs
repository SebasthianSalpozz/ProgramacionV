module StringPermute where

-- Función para permutar los caracteres de un String
stringPermute :: String -> String
stringPermute [] = []
stringPermute (x:y:xs) = y : x : stringPermute xs
stringPermute (x:xs) = x : stringPermute xs