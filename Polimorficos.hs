module Maisn where

type DiaLiteral = String
type DiaNumeral = Int
type MesNumeral =Int
type AñoNumeral = Int
type DiasdelMes = Int
queDiaes :: DiaNumeral -> DiaLiteral
queDiaes x = case x of
    1 -> "Domingo"
    2 -> "Lunes"
    3 -> "Martes"
    4 -> "Miercoles"
    5 -> "Jueves"
    6 -> "Viernes"
    7 -> "Sabado"
    _ -> "No es un dia de la semana"

diadelMes :: MesNumeral -> AñoNumeral -> DiasdelMes
diadelMes m a = let 
                    esBiciesto = añoBiciesto a
                in case m of
                    1 -> 31
                    2 -> if esBiciesto then 29 else 28
                    3 -> 31
                    4 -> 30
                    5 -> 31
                    6 -> 30
                    7 -> 31
                    8 -> 31
                    9 -> 30
                    10 -> 31
                    11 -> 30
                    12 -> 31
                    _ -> 0
            where
                añoBiciesto :: AñoNumeral -> Bool
                añoBiciesto a =  (a `mod` 4 == 0 && a `mod` 100 /= 0) || a `mod` 400 == 0



-- (mod a 4 == 0 && mod a 100 /= 0) || mod a 400 == 0