import Data.Char
import Data.Ord

type Bit = Int

-- Convierte un entero a una lista de bits
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

-- Convierte una lista de bits a un entero
bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

-- Asegura que una lista de bits tenga una longitud de 8
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

-- A higher-order function unfold that encapsulates a simple pattern of recursion for producing a list can be defined as follows:
unfold p h t x | p x = []
                | otherwise = h x : unfold p h t (t x)
-- That is, the function unfold p h t produces the empty list if the predicate p is true for the argument value x; otherwise, it 
--produces a list whose first element is the result of applying the function h to x, and whose remaining elements are produced by recursively 
--calling unfold on the value t x. For example, the function intToBin can be rewritten more compactly using unfold as follows:
-- intToBin = unfold (== 0) (`mod` 2) (`div` 2)
-- Redefine the functions chop8, map f and iterate f using unfold.

myMap :: (a -> b) -> [a] -> [b]
myMap f = unfold null (f . head) tail

myIterate :: (a -> a) -> a -> [a]
myIterate f = unfold (\_ -> False) id f

--Modify the binary string transmitter example to detect simple transmission errors using the concept of parity bits. That is, each
--8-bit binary number produced during encoding is extended with a parity bit, set to one if the number contains an odd number of ones, and to zero otherwise. 
--In turn, each resulting 9-bit binary number consumed during decoding is checked to ensure that its parity bit is correct, with the parity bit being discarded if 
--this is the case, and a parity error being reported otherwise.

parityBit :: [Bit] -> [Bit]
parityBit bits = bits ++ [if odd (sum bits) then 1 else 0]

parityCheck :: [Bit] -> [Bit]
parityCheck bits | odd (sum bits) = error "Parity error"
                 | otherwise = init bits

myEncode :: String -> [Bit]
myEncode = concat . map (parityBit . make8 . int2bin . ord)

myDecode :: [Bit] -> String
myDecode = map (chr . bin2int . parityCheck) . chop9

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

myTrasmit :: String -> String
myTrasmit = myDecode . channel . myEncode

-- Test your new string transmitter program from the previous exercise using a faulty communication channel that forgets the first bit, 
--which can be modeled using the tail function on lists of bits.

faultyChannel :: [Bit] -> [Bit]
faultyChannel = tail

-- Define a function altMap :: (a -> b) -> (a -> b) -> [a] -> [b] that alternately applies its two argument functions to successive elements in a list,
--in turn about order. For example:

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g = map (\(x, y) -> if even x then f y else g y) . zip [0..]

