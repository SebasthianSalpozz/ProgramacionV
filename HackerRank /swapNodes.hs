module SwapNodes where
  
data Btree a = Empty
              |Null
              | Node a (Btree a) (Btree a)
              deriving (Show)

tree :: Btree Int
tree = Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)

addNode :: Int -> Int -> Btree Int -> Btree Int
addNode _ _ Null = Null
addNode x depth (Node val left right)
    | depth == 1 = Node x (addNode x (depth-1) left) right
    | depth > 1 = Node val (addNode x (depth-1) left) (addNode x (depth-1) right)
    | otherwise = Node val left right



-- QUiero que me crees la insercion de los datos tomando en cuenta que si nos pasan -1 es un valor nulo 
-- y que vamos a buscar en la profundidad del arbol y al de menor profundidad le ponemos los valores siempre 
-- y cuando no sean nulos y si la profundida es la misma siempre se toma la insercion al lado izquierdo
-- la funcion sera addNode:: Int -> Int -> Btree Int -> Btree Int  