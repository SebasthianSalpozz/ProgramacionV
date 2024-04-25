
import System.Random
import GHC.Float (leDouble)
import Control.Applicative (Alternative(empty))
import Control.Arrow (ArrowChoice(right))

data Estudiante = Estudiante
  { nombres :: String,
    apellidos :: String,
    salio :: Bool,
    resolvioEjercicio :: Bool
  }
  deriving (Show)

data Tree a = Empty
            | Leaf a 
            | Node (Tree a )a (Tree a)
            deriving  (Show)

data List a = Vacio 
            |Siguiente a (List a)
            deriving (Show)
instance Eq a => Eq (Tree a)where
  (Leaf x )== (Leaf y ) = x == y
  (Node left1 val1 right1)== (Node left2 val2 right2)= (left1 == left2)&&(val1==val2)&& (right1 == right2)
  _ == _ = False

arbol :: Tree Int 
arbol = Node (Node (Leaf 1) 2 (Leaf 3)) 4 (Node (Leaf 5) 6 (Leaf 7))


myList :: List String 
myList = Siguiente "Manzanas" (Siguiente "Limones " Vacio)

myListLength :: List a -> Int
myListLength Vacio = 0
myListLength (Siguiente _ xs) = 1 + myListLength xs

estudiantes :: [Estudiante]
estudiantes =
  [ Estudiante "Juan Carlos" "Perez Lopez" False False,
    Estudiante "Maria" "Gonzalez" False False,
    Estudiante "Pedro" "Perez" False False,
    Estudiante "Luis" "Gonzalez" False False,
    Estudiante "Ana" "Lopez" False False,
    Estudiante "Jose" "Perez" False False,
    Estudiante "Luisa" "Gonzalez" False False,
    Estudiante "Carlos" "Lopez" False False,
    Estudiante "Juan" "Perez" False False,
    Estudiante "Maria" "Gonzalez" False False
  ]

seleccionarEstudiante :: [Estudiante] -> IO Estudiante
seleccionarEstudiante estudiantes = do
  indice <- randomRIO (0, length estudiantes - 1)
  return $ estudiantes !! indice
  where
    filtrarEstudiantes :: [Estudiante] -> [Estudiante]
    filtrarEstudiantes = filter  (\x -> not (salio x) && not (resolvioEjercicio x))

addNode :: Ord a=> Tree a -> a -> Tree a
addNode (Leaf x) new =  if new < x  
                        then Node (Leaf new) x Empty
                        else Node Empty x (Leaf new)
addNode (Node left x right)y 
      |y<= x = Node (addNode left y) x right
      |otherwise = Node left x (addNode right y)

inOrden :: Tree a -> [a]
inOrden (Leaf x) = [x]
inOrden (Node left x right) =  

postOrden :: Tree a -> [a]
postOrden (Leaf x) = [x]
postOrden (Node left x right)= 

preOrden :: Tree a -> [a]
preOrden (Leaf x) = [x]
preOrden (Node left x right)= 