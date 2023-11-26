--Zad 1
liniowa :: Int -> Int
liniowa x = x

kwadratowa :: Int -> Int
kwadratowa x = x * x

jestPunktemStalym :: (Eq a) => a -> (a -> a) -> Bool
jestPunktemStalym x f = if f x == x then True else False

--Zad 2
ilosum :: (Num a, Show a, Ord a) => a -> a -> String
ilosum x y | x * y > x + y = "Iloczyn " ++ show x ++ " oraz " ++ show y ++ " jest wiekszy od sumy"
           | otherwise = "Iloczyn " ++ show x ++ " oraz " ++ show y ++ " nie jest wiekszy od sumy"

--Zad 4
dzielnik :: Int -> Int -> Bool
dzielnik x y | y `mod` x == 0 = True
             | otherwise = False

dzielniki :: Int -> [Int]
dzielniki y = [x | x <- [1..(y `div` 2)], dzielnik x y == True]

--Zad 5
suma :: [Int] -> Int
suma [] = 0
suma [x] = x + suma []
suma (x:xs) = x + suma xs

i :: [(Int, Int)] -> Int
i [] = 0
i ((x, y):xs) = if x >= 0 && y >= 0 then 1 + i xs else i xs

ii :: [(Int, Int)] -> Int
ii [] = 0
ii ((x, y):xs) = if x >= 0 && y <= 0 then 1 + ii xs else ii xs

iii :: [(Int, Int)] -> Int
iii [] = 0
iii ((x, y):xs) = if x <= 0 && y <= 0 then 1 + iii xs else iii xs

iv :: [(Int, Int)] -> Int
iv [] = 0
iv ((x, y):xs) = if x <= 0 && y >= 0 then 1 + iv xs else iv xs

ktoraCwiartka :: [(Int,Int)] -> Int
ktoraCwiartka xs | i xs == maximum list = 1
                 | ii xs == maximum list = 2
                 | iii xs == maximum list = 3
                 | otherwise = 4
                where 
                    list = [i xs, ii xs, iii xs, iv xs]



--Zad 6
podlisty :: [Integer] -> [[Integer]]
podlisty [] = []
podlisty xs | n < length xs - 1 = 
podlisty xs | otherwise = xs : podlisty mniejsza n + 1 hs
            where 
                mniejsza n xs = m splitAt n - 1 xs
                m (xs,ys) = xs ++ drop 0 ys
-- pokolei usuwać elementy z listy i mergeować a później to samo dla tabeli o 1 mniejszej

--Zad 7
przeksztalcListe :: (Int -> Int) -> [Int] -> [Int]
przeksztalcListe f [x] = f x : []
przeksztalcListe f (x:xs) = f x : przeksztalcListe f xs