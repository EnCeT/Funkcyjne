import qualified Graphics.Win32 as Zad
--Zad.1
fl:: Float -> [Float] -> Float
fl n x = foldl (/) n x

fr:: Float -> [Float] -> Float
fr n x = foldr (/) n x

isiteq:: Float -> Float -> Bool
isiteq a b = if a == b then True else False

--Zad.2
re:: [a] -> [a]
re = foldl (\a b -> b:a) [] 

rew:: [a] -> [a]
rew = foldr (\a b -> b ++[a]) []

--Zad.3
ma:: (a -> a) -> [a] -> [a]
ma f x = foldr (\a b -> (f a) : b) [] x

maa:: (a -> a) -> [a] -> [a]
maa f x = foldl (\a b -> a ++ [(f b)]) [] x

--Zad.4
pot :: Int -> Int -> Int
pot x 0 = 1 
pot x n = x * pot x (n - 1)

tab :: Int -> Int -> [Int]
tab x 0 = [1]
tab x n = [pot x n] ++ tab x (n - 1)

doDziesietnego :: Int -> [Int] -> Int
doDziesietnego n x = foldr (+) 0 (zipWith (*) x (tab n ((length x) - 1)))

--Zad.5
punktWKtorejCwiartce :: (Int, Int) -> [Int] -> [Int]
punktWKtorejCwiartce (x, y) [a, b, c, d] | x == 0 && y == 0 = [a + 1, b + 1, c + 1, d + 1]
                            | x == 0 && y > 0 = [a + 1, b + 1, c, d]
                            | x == 0 && y < 0 = [a, b, c + 1, d + 1]
                            | x > 0 && y == 0 = [a + 1, b, c, d + 1]
                            | x < 0 && y == 0 = [a, b + 1, c + 1, d]
                            | x > 0 && y > 0 = [a + 1, b, c, d]
                            | x > 0 && y < 0 = [a, b, c, d + 1]
                            | x < 0 && y > 0 = [a, b + 1, c, d]
                            | x < 0 && y < 0 = [a, b, c + 1, d]
                            
ktoraCwiartka' :: [Int] -> Int
ktoraCwiartka' [a,b,c,d] | a >= b && a >= c && a >= d = 1
                | b >= a && b >= c && b >= d = 2
                | c >= a && c >= b && c >= d = 3
                | d >= a && d >= b && d >= c = 4

ktoraCwiartka :: [(Int, Int)] -> Int
ktoraCwiartka list = ktoraCwiartka'( foldl (\l x -> punktWKtorejCwiartce x l) [0, 0, 0, 0] list) 

--Zad.6
dlaKazdego :: (a -> Bool) -> [a] -> Bool
dlaKazdego f x = foldl (&&) True (map (f) x)

istnieje :: (a -> Bool) -> [a] -> Bool
istnieje f x = foldl (||) False (map (f) x)

--Zad.7
