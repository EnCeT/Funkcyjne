-- zad 1
type Point = (Double, Double)

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2)

getDistance :: (Point, Point, Double) -> Double
getDistance (_, _, d) = d

distList :: [Point] -> [(Point, Point, Double)]
distList [] = []
distList (x:xs) = [(x, y, distance x y) | y <- xs] ++ distList xs

minDist :: [Point] -> (Point, Point, Double)
minDist xs = foldl (\acc z -> if getDistance acc > getDistance z then z else acc) y ys
    where (y:ys) = distList xs

-- zad 2
data Tree a = Empty | Node a (Tree a) (Tree a)
findPathHelper :: Eq a => a -> Tree a -> [a] -> [a]
findPathHelper _ Empty _ = []
findPathHelper x (Node a b c) list
    | x == a = a:list
    | otherwise = findPathHelper x b (a:list) ++ findPathHelper x c (a:list)

findPath :: Eq a => a -> Tree a -> [a]
findPath _ Empty = []
findPath x n = reverse (findPathHelper x n [])

-- zad 3
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort (filter (x >) xs) ++ [x] ++ quickSort (filter (x <=) xs)

cp :: [Integer] -> [Integer]
cp xs = [toInteger y | y <- [1..length xs], quickSort (take y xs) == [1..toInteger y]]