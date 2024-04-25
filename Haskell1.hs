import Data.Char (ord)
import Distribution.Simple.Program.HcPkg (list)
import Control.Monad.RWS.Lazy (MonadState(get))
nextNum n = n + 1

square n = n*n

cadena = "Hola Sebas"

suma :: Int-> Int -> Int
suma a b= a + b

tupla :: Char -> (Char, Int)
tupla x = (x, ord x) 

sumaTupla :: (Int, Int, Char) -> Int
sumaTupla (a,b, c) = a+b

sumaLista :: [Int] -> Int
sumaLista xs = sum xs

sumaLista' ::[Int]-> Int
sumaLista'= sum

lista :: [Int]
lista = [1..30]

esPar :: Int -> Bool
esPar x = even x

filtrarPares ::[Int] -> [Int]
filtrarPares xs= filter esPar xs 

mySplit :: Int -> [Int] ->([Int], [Int])
mySplit n xs = (take n xs, drop n xs)

myAbs :: Int -> Int
myAbs n = if n >= 0 then n else -n 

validPositiveNUmber :: Int -> Int
validPositiveNUmber n = if n< 0 then -1 else 
                                        if n == 0 then 0 else 1

--Guard
myAbs' :: Int -> Int
myAbs' n | n >= 0 = n 
         | otherwise = -n 

validPositiveNUmber' ::Int ->Int
validPositiveNUmber' n|n<0 = -1
                      |n==0 = 0 
                      |otherwise = 1

--Pattern Matching
negar :: Bool-> Bool
negar True = False
negar False = True

--patter Mattching
esUno :: Int -> Bool
esUno 1 = True
esUno _ = False

add:: Int->(Int -> Int)
add = \x -> (\y -> x + y)


myFunctions :: [(Int -> Int -> Int)]
myFunctions= [(+), (-), (*), div]

getFunction :: Char -> (Int->Int -> Int)
getFunction e | e == '+' =  myFunctions !! 0
              | e == '-' =  myFunctions !! 1
              | e == '*' =  myFunctions !! 2
              | e == '/' =  myFunctions !! 3
              | otherwise = myFunctions !! 4

---myExp :: Char -> (Int -> Int-> Int)
---myExp e = getFunction e------

-- Curried Function __Explicar mas ejemplos__
-- ((multThree 1) 3) 4
multThree :: Int -> (Int -> (Int -> Int))
multThree x y z = x*y*z
 
--[x^2 | x E (1..5)] para todas las x elevar a 2 

-- [(x, y) | x <- [1..5], y <- [1..5]]

--Funciones Polimorficas
--Son funciones que aceptan varios tipos de valores 
fac :: Int -> Int
fac 0=1 
fac n=n*fac(n-1)

myLength :: [a] -> Int
myLength []=0
myLength (_:xs) = 1 + myLength xs

inserVal :: Ord  a => a -> [a] -> [a] 
inserVal x []= [x]
inserVal x (y:ys)   | x<= y = x: y: ys
                    | otherwise = y: inserVal x ys

myZip ::[a]->[b]->[(a,b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x,y): myZip xs ys

mySum :: Num a => [a] -> a
mySum = foldr (+) 0

myFac :: Num a => [a] -> a
myFac = foldr (*) 1

--(.) Funcion de Compocicion 
twiceSquare :: Int -> Int
twiceSquare = square . square 
