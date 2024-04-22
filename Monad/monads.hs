-- What is a monad  
-- Una mónada en Haskell es una clase de tipos que proporciona una forma de secuenciar cálculos. 
-- Le permite encadenar múltiples cálculos mientras maneja efectos como el estado

-- para un monad se debe definir el operador de secuencia el >>= y el return

import Control.Monad.State
import Control.Monad (replicateM)

nextRow :: [Integer] -> [Integer]
nextRow row = zipWith (+) (0:row) (row ++ [0])


pascalTriangle :: Int -> [[Integer]]
pascalTriangle n = evalState (replicateM n generateRow) [1]
    where
        generateRow :: State [Integer] [Integer]
        generateRow = do
            row <- get
            put (nextRow row)
            return row

main :: IO ()
main = do
    let n = 5
    let triangle = pascalTriangle n
    mapM_ print triangle