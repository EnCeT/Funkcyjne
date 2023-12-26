--Mapy
squareMe list = map (\x -> x * x) list

doubleMe list = map (\x -> 2 * x) list

--all bin with map
func :: [[Int]] -> [[Int]]
func [] = []
func x = map (\x -> (x ++ [1])) x ++ map (\x -> (x ++ [0])) x

func2 :: Int -> [[Int]] -> [[Int]]
func2 1 y = y
func2 n y = func2 (n - 1) (func y)

allBin :: Int -> [[Int]]
allBin 0 = []
allBin n = func2 n [[0], [1]]

--reverse - funkcja wbudowana
reverseAll list = concatMap reverse list

--wypłaszczenie listy
flatten list = concatMap (\x -> x) list

--zad2 rozwiązanie równania przez zakresy lub filtered
solveFilter n = filter (\(x, y, z) -> x * x - 2 * y * y == z * z * z * z * z) [(x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n]]

solveRange n = [(x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n], x * x - 2 * y ** 2 == z ** 5, even x, even y, even z]

--dzielniki liczby n
divisorsRange :: Int -> [Int]
divisorsRange 1 = [1]
divisorsRange n = [x | x <- [1 .. n], n `mod` x == 0, x /= n]

divisorsFilter :: Int -> [Int]
divisorsFilter 1 = [1]
divisorsFilter n = filter (\x -> x `mod` n == 0) [x | x <- [1 .. n]]

divisorsSum :: [Int] -> Int
divisorsSum [] = 0
divisorsSum (x : xs) = x + divisorsSum xs

prefectNum n = [x | x <- [1 .. n], x == divisorsSum (divisorsRange x)]

addN :: Int -> Int -> [Int] -> [Int]
addN n k x = take (n - 1) x ++ [k] ++ drop (n - 1) x

perm :: Int -> [[Int]]
perm 1 = [[1]]
perm n = insert (perm (n - 1)) (n - 1) n
  where
    insert :: [[Int]] -> Int -> Int -> [[Int]]
    insert x 0 b = map (\x -> b : x) x
    insert x a b = map (\x -> insertAtPos x a b) x ++ insert x (a - 1) b
    insertAtPos :: [Int] -> Int -> Int -> [Int]
    insertAtPos x a b = take a x ++ [b] ++ drop a x

primeDel :: Int -> [Int] -> [Int]
primeDel n x = filter (\x -> x `mod` n /= 0 || x == n) x

primesbuf :: [Int] -> [Int]
primesbuf [] = []
primesbuf (x : xs) = [x] ++ primesbuf (primeDel x xs)

primes :: Int -> [Int]
primes n = primesbuf [x | x <- [2 .. n]]

--subsets
subsets :: [Int] -> [[Int]]
subsets [] = [[]]
subsets (x : xs) = subsets xs ++ map (x :) (subsets xs)

subsets1 :: [Int] -> [[Int]]
subsets1 [] = [[]]
subsets1 (x : xs) = filter (\x-> length x>1)( subsets xs ++ map (x :) (subsets xs))

--square root
funkc :: Double -> Double -> Double
funkc a x = (1 / 2) * (x + a / x)

funkc2 :: Double -> [Double]
funkc2 c = iterate (funkc c) c

finalresult :: Int -> [Double] -> [Double]
finalresult 0 _ = []
finalresult a (x : xs) = [x] ++ finalresult (a - 1) xs

getPrecision :: Double -> Int -> Int
getPrecision 1 x = x
getPrecision d i = getPrecision (d * 10) (i + 1)

squareRoot :: Double -> Double -> Double
squareRoot x e = last (finalresult ((getPrecision e 0) + 2) (funkc2 x))

--thue morse
h :: Char -> String
h s
  | s == 'a' = "ab"
  | s == 'b' = "ba"

hstr :: String -> String
hstr [] = []
hstr (x : xs) = h x ++ hstr xs

countSize :: Int -> Int
countSize n = last [x | x <- map (2 ^) [0 .. n], x < n]

logg2 :: Int -> Int -> Int
logg2 0 m = m
logg2 n m = logg2 (n `div` 2) (m + 1)

getList :: Int -> [String] -> [String]
getList 0 _ = []
getList n (x : xs) = [x] ++ getList (n - 1) xs

thueMorse :: Int -> String
thueMorse n = last (getList (logg2 (countSize n) 0) (iterate hstr "a"))

listMerger :: [[Int]]->[[Int]]->[[Int]]
listMerger [][] = []
listMerger (x:xs) (y:ys)= (x++y) : listMerger xs ys

findElem:: [Int]->Int-> Bool
findElem list item = if (length (filter (\x->x==item) list)>0) then True else False

removeDubs :: [Int] -> [Int]
removeDubs  = foldl (\acc x -> if not (findElem acc x)  then acc++[x] else acc) [] 