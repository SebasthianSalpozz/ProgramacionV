data Shape = Circle Float | Rect Float Float deriving (Show)

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

data MyMaybe a = Mana | Hay a deriving (Show)


safeDiv :: Float -> Float -> MyMaybe Float
safeDiv _ 0 = Mana
safeDiv x y = Hay (x / y)

getValue :: MyMaybe Int -> Int
getValue Mana = 0
getValue (Hay x) = x
