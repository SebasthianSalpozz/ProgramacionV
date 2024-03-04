import Language.Haskell.TH (safe)
--Chapter 3
--What are the types of the following functions?
second xs = head (tail xs)
--Type: second :: [a] -> a

swap (x, y) = (y, x)
--Type: swap :: (b, a) -> (a, b)

pair x y = (x, y)
--Type: pair :: a -> b -> (a, b)

double x = x * 2
--Type: double :: Num a => a -> a
--(Uses the Num type class for multiplication)

palindrome xs = reverse xs == xs
--Type: palindrome :: Eq a => [a] -> Bool
--(Uses the Eq type class for equality comparison)

twice a x = a (a x)
-- Type: twice :: (t -> t) -> t -> t

--Why is it not feasible in general for function types to be instances of the Eq class? When is it
--feasible? Hint: two functions of the same type are equal if they always return equal results for
--equal arguments
--No es factible que los tipos de funciones sean instancias de la clase 'Eq' porque determinar la igualdad de funciones puede resultar complicado. 
--Las funciones pueden tener efectos secundarios o depender de estados externos,lo que dificulta compararlas únicamente por sus resultados. 
--La viabilidad surge cuando las funciones siempre devuelven los mismos resultados para los mismos argumentos y no dependen de estados externos.

--Exercise Chapter 4
--Without using any other library functions or operators, show how the meaning of the following
--pattern matching definition for logical conjunction && can be formalised using conditional
--expressions:
-- True && True = True
-- _ && _ = False
--Answer:
myAnd :: Bool -> Bool -> Bool
myAnd x y = if x == True then
            if y == True then True
            else False
            else False
--Do the same for the following alternative definition, and note the difference in the number of
--conditional expressions that are required:
-- True && b = b
-- False && _ = False
myAnd2 :: Bool -> Bool -> Bool
myAnd2 x y = if x == True then y
            else False

--Show how the meaning of the following curried function definition can be formalised in terms of
--lambda expressions:
--mult :: Int -> Int -> Int -> Int
--mult x y z = x * y * z
--Answer:
mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> x * y * z))

--The Luhn algorithm is used to check bank card numbers for simple errors such as mistyping a
--digit, and proceeds as follows:
-- consider each digit as a separate number;
-- moving left, double every other number from the second last;
-- subtract 9 from each number that is now greater than 9;
-- add all the resulting numbers together;
-- if the total is divisible by 10, the card number is valid.
--Define a function luhnDouble :: Int -> Int that doubles a digit and subtracts 9 if the result is
--greater than 9.
luhnDouble :: Int -> Int
luhnDouble x | x * 2 > 9 = x * 2 - 9
             | otherwise = x * 2

--Using luhnDouble and the integer remainder function mod, define a function 
--luhn :: Int -> Int -> Int -> Int -> Bool that decides if a four-digit bank card number is valid.
luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = if (luhnDouble a + b + luhnDouble c + d) `mod` 10 == 0 then True
               else False

--Exercise Chapter 5
--A positive integer is perfect if it equals the sum of all of its factors, excluding the number itself.
--Using a list comprehension and the function factors, define a function 
--perfects :: Int -> [Int] that returns the list of all perfect numbers up to a given limit.
myFactors :: Int -> [Int]
myFactors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects 0 = []
perfects n = [x | x <- [1..n], x == sum (init (myFactors x))]

--Show how the list comprehension [(x,y) | x <- [1,2], y <- [3,4]] with two generators
--can be re-expressed using two comprehensions with single generators. Hint: nest one
--comprehension within the other and make use of the library function concat :: [[a]] -> [a].
myConcat :: [[a]] -> [a]
myConcat xs = foldr (++) [] xs
--Redefine the function positions using the function find.
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']

--The scalar product of two lists of integers xs and ys of length n is given by the sum of the products
--of corresponding integers:
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x * y | (x, y) <- zip xs ys]

--Exercise Chapter 6
--Using the recursive definitions given in this chapter, show how length
--[1,2,3,4,5], and init [1,2,3] are evaluated [1,2,3] drop 3 [1,2,3,4,5] 
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myInit :: [a] -> [a]
myInit [_] = []
myInit (x:xs) = x : myInit xs

myDrop :: Int -> [a] -> [a]
myDrop 0 xs = xs
myDrop _ [] = []
myDrop n (_:xs) = myDrop (n-1) xs

--Without looking at the definitions from the standard prelude, define the following library functions on lists using recursion.
--Decide if all logical values in a list are True:
myAnd3 :: [Bool] -> Bool
myAnd3 [] = True
myAnd3 (x:xs) = x && myAnd3 xs

--Concatenate a list of lists:
myConcat2 :: [[a]] -> [a]
myConcat2 [] = []
myConcat2 (x:xs) = x ++ myConcat2 xs

--Produce a list with n identical elements:
myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate n x = x : myReplicate (n-1) x

--Select the nth element of a list:
mySelect :: [a] -> Int -> a
mySelect (x:_) 0 = x
mySelect (_:xs) n = mySelect xs (n-1)

--Decide if a value is an element of a list:
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys) = x == y || myElem x ys

--Define a recursive function merge :: Ord a => [a] -> [a] -> [a] that merges two sorted
--lists to give a single sorted list.
myMerge :: Ord a => [a] -> [a] -> [a]
myMerge xs [] = xs
myMerge [] ys = ys
myMerge (x:xs) (y:ys) | x <= y = x : myMerge xs (y:ys)
                      | otherwise = y : myMerge (x:xs) ys

--Using merge, define a function msort :: Ord a => [a] -> [a] that implements merge sort, in
--which the empty list and singleton lists are already sorted, and any other list is sorted by mergingtogether the 
--two lists that result from sorting the two halves of the list separately.
--Hint: first define a function halve :: [a] -> ([a],[a]) that splits a list into two halves whose
--lengths differ by at most one
myHalve :: [a] -> ([a],[a])
myHalve xs = (take n xs, drop n xs)
             where n = length xs `div` 2

myMsort :: Ord a => [a] -> [a]
myMsort [] = []
myMsort [x] = [x]
myMsort xs = myMerge (myMsort ys) (myMsort zs)
             where (ys, zs) = myHalve xs

--Using the five-step process, define the library functions that 
--calculate the sum of a list of numbers
--take a given number of elements from the start of a list
--select the last element of a non-empty list
mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs

myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake _ [] = []
myTake n (x:xs) = x : myTake (n-1) xs

myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs

