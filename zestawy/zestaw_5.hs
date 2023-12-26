fun1 :: [String] -> [String]
fun1 = map (filter (\x -> x `elem` ['a'..'z']))

fun2 :: [String] -> Int
fun2 x = sum (map length (filter (\y -> y == reverse y) x))

fib :: (Integer,Integer) -> [(Integer,Integer)]
fib = iterate (\(x, y) -> (y, x+y))

fun3 :: [a] -> Int
fun3 x = sum (map (const 1) x)

fun4 :: Char -> Char -> Integer -> [String]
fun4 _ _ 0 = [""]
fun4 x y n = map (x:) (fun4 x y (n-1)) ++ map (y:) (fun4 x y (n-1))

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort (filter (x >) xs) ++ [x] ++ quickSort (filter (x <=) xs)

myZip :: [a] -> [b] -> [(a,b)]
myZip = zipWith (,)
myMap :: (a -> b) -> [a] -> [b]
myMap f xs = zipWith (\x y -> f x) xs xs

sito :: Integer -> [Integer] -> [Integer]
sito 1 xs = xs
sito n xs = sito (n-1) (filter (\x -> x == n || x `mod` n /= 0) xs)
eratosthenes :: Integer -> [Integer]
eratosthenes n = sito n [2..n]