type Point = (Double, Double)

-- Calcula el producto cruzado de dos vectores
-- | Calculates the signed area of a triangle formed by three points.
-- The sign of the area determines whether the triangle is oriented clockwise or counterclockwise.
-- If the area is positive, the triangle is counterclockwise.
-- If the area is negative, the triangle is clockwise.
-- If the area is zero, the points are collinear.
area :: Point -> Point -> Point -> Double
area (x1, y1) (x2, y2) (x3, y3) = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)

-- | Checks if a polygon is concave.
-- A polygon is concave if there exists at least one vertex where the signed area of the adjacent triangles is negative.
isConcave :: [Point] -> Bool
isConcave points = any (< 0) crossProducts
    where
        n = length points
        crossProducts = [area p1 p2 p3 | i <- [0..n-1], let p1 = points !! i, let p2 = points !! ((i + 1) `mod` n), let p3 = points !! ((i + 2) `mod` n)]

-- Ejemplo de uso
main :: IO ()
main = do
    let polygon = [(0, 0), (1, 0), (1, 1), (0, 1)] 
    let isConcavePolygon = isConcave polygon
    putStrLn $ show isConcavePolygon