--Zad 1
f :: Float -> Float -> Float
f 0 0 = 69
f x y = (x*x + y*y) / 2*x*y

fx :: Float -> Float
fx x = f x 1

--Zad 2
m3 :: Int -> Int
m3 x = x * 2

m2 :: Int -> Int
m2 x = x * 3

sumaWartości :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> Int
sumaWartości m3 m2 x y = m3 x + m2 y

sumaW :: Int -> Int -> Int
sumaW x y = sumaWartości m3 m2 x y

--Zad 3
ocena :: Double -> String
ocena x | x == 2.0 = "niezaliczone"
        | x == 5.0 = "brawo!"
        | otherwise = "wpisane masz " ++ show x

--Zad 4
stirling :: Int -> Int -> Int
stirling x y | x == 0 && y == 0 = 1
             | y == 0 = 0
             | x == y = 1
             | otherwise = (x - 1) * stirling (x-1) y + stirling (x - 1) (y - 1)

--Zad 5
iloczynListy :: [Integer] -> Integer
iloczynListy [] = 1
iloczynListy x = head x * iloczynListy (tail x)

--Zad 6
--funkcja łacząca
merge :: [Integer] -> [Integer] -> [Integer]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = if x < y then y :merge (x:xs) ys else x :merge xs (y:ys)

--funkcja dzieląca
d :: [a] -> ([a], [a])
d xs = splitAt (length xs `div` 2) xs

mergeSort :: [Integer] -> [Integer]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort l) (mergeSort r)
              where
                (l,r) = d xs

--Zad 7
dzielnik :: Int -> Int -> Bool
dzielnik x y | y `mod` x == 0 = True
             | otherwise = False

divList :: Int -> [Int]
divList y = [x | x <- [1..(y `div` 2)], dzielnik x y == True]

czyDoskonala :: Int -> Bool
czyDoskonala x = if (sum (divList x)) == x then True else False

