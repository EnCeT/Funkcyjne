-- zad 1
transform :: Int -> Int
transform n
    | n < 10 = n
    | otherwise =  mod n 10 + transform (div n 10)

isSeven :: Int -> Bool
isSeven n
    | n == 7 = True
    | n < 10 = False
    | otherwise = isSeven (transform n)

seven :: Int -> [Int]
seven n = take n (filter isSeven [1..])

-- zad 2
palidromeGenerator :: Int -> [String]
palidromeGenerator n 
    | n<2 = [""]
    | otherwise = map (\x -> "a" ++ x ++ "a") (palidromeGenerator (n-2)) ++ map (\x -> "b" ++ x ++ "b") (palidromeGenerator (n-2))

count :: Char -> [Char] -> Int
count n = foldl (\acc x -> if n == x then acc + 1 else acc) 0

balancedPalidrome :: Int -> [String]
balancedPalidrome n
    | n `mod` 4 > 0 = [""]
    | otherwise = filter (\x -> count 'a' x == count 'b' x) (palidromeGenerator n)

-- zad 3
data Mb a = Mb [a] [a]

dnp :: Mb a -> a -> Mb a
dnp (Mb xs ys) x = Mb (x:xs) ys

dnk :: Mb a -> a -> Mb a
dnk (Mb xs ys) y = Mb xs (y:ys)

mb2list :: Mb a -> [a]
mb2list (Mb xs ys) = xs ++ reverse ys

ull :: Mb a -> Mb a
ull (Mb _ ys) = Mb [] ys

ulr :: Mb a -> Mb a
ulr (Mb xs _) = Mb xs []