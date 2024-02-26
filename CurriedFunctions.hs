{-
Las funciones curried son funciones que están parcialmente aplicadas 
toman múltiples argumentos y devuelven una nueva función que espera los argumentos
restantes. 
En Haskell, todas las funciones son curried de forma predeterminada.
-}
--Ejemplo 1 suma
suma :: Int -> Int -> Int
suma x y = x + y

suma5 :: Int -> Int
suma5 = suma 5

--ejemplo 2 resta
resta :: Int -> Int -> Int
resta x y = x - y

resta3 :: Int -> Int
resta3 = resta 3

--Ejemplo 3 Multiplicacion
multiplicacion :: Int -> Int -> Int
multiplicacion x y = x * y


doble :: Int -> Int
doble = multiplicacion 2


--Ejemplo 4 Division
division :: Float -> Float -> Float
division x y = x / y

tercio :: Float -> Float
tercio = division 3.0

--Ejemplo 5 Potencia
potencia :: Int -> Int -> Int
potencia x y = x ^ y

cubo :: Int -> Int
cubo = potencia 3




