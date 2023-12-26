{- Usuwa duplikaty z listy -}
deleteDuplicates :: Eq a => [a] -> [a]
deleteDuplicates l = deleteDuplicates' l []
  where
    deleteDuplicates' [] _ = []
    deleteDuplicates' (x : xs) ls
      | x `elem` ls = deleteDuplicates' xs ls
      | otherwise = x : deleteDuplicates' xs (x : ls)


{- Ile razy wystapil dany element w liscie -}
countInList :: Eq a => a -> [a] -> Int
countInList _ [] = 0
countInList n (x : xs)
  | n == x = 1 + countInList n xs
  | otherwise = countInList n xs


{- Zwraca liste par (element, liczba wystapien) dla kazdego unikatowego elementu -}
countElementsInList :: Eq a => [a] -> [(a, Int)]
countElementsInList xs = [(e, countInList e xs) | e <- deleteDuplicates xs]


-- typ drzewa
data Tree a = Empty | Node a (Tree a) (Tree a)

-- czy wszystkie elementy w drzewie sa parzyste 
-- (umieszczam bardziej jako przypomnienie struktury funkcji dzialajacej na drzewie)
czyWszystkieParzyste :: Tree Int -> Bool
czyWszystkieParzyste Empty = True
czyWszystkieParzyste (Node x t1 t2) = even x && czyWszystkieParzyste t1 && czyWszystkieParzyste t2

-- przeszukania drzewowe
preorder :: (Ord a) => Tree a -> [a]
preorder Empty = []
preorder (Node x t1 t2) = x : preorder t1 ++ preorder t2

inorder :: (Ord a) => Tree a -> [a]
inorder Empty = []
inorder (Node x t1 t2) = inorder t1 ++ [x] ++ inorder t2

postorder :: (Ord a) => Tree a -> [a]
postorder Empty = []
postorder (Node x t1 t2) = postorder t1 ++ postorder t2 ++ [x]


-- fibonacci
fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)


-- sortowanie (dla typu majacego Ord)
insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort [x] = [x]
insertionSort (x:xs) = insert $ insertionSort xs
    where insert [] = [x]
          insert (y:ys)
              | x < y = x : y : ys
              | otherwise = y : insert ys

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]


-- zamiana liczby dziesietnej na binarna
toBinary :: Int -> [Int]
toBinary 0 = [0]
toBinary n = toBinary (n `quot` 2) ++ [n `rem` 2]

toBinaryFixed :: Int -> [Int]
toBinaryFixed 0 = [0]
toBinaryFixed n = tail (toBinary n)


-- grupowanie elementow listy po n elelmntow
group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = take n l : group n (drop n l)
  | otherwise = error "Negative or zero n"


-- wstawianie elementu do listy n n-te miejsce
insertAt :: [a] -> a -> Int -> [a]
insertAt [] elem pos = [elem]
insertAt (x:xs) elem pos
    | pos == 0 = elem : x : xs
    | pos > 0 = x : insertAt xs elem (pos - 1) 
    | otherwise = x : insertAt xs elem (pos + length (x:xs))


-- zwraca liste cyfr liczby podanej argumentem
listOfDigits :: Integer -> [Integer]
listOfDigits n
  | n >= 0 && n <= 9 = [n]
  | otherwise = listOfDigits (n `div` 10) ++ [n `mod` 10]


-- nieskonczona lista od 1 typu Integer (zwykla jest typu Int, nie zawsze mozna)
infListOfInteger :: [Integer]
infListOfInteger = [1..]


-- np. dla inputu [1,2,3,4,5] daje [[2,3,4,5], [3,4,5], [4,5], [5]]
generateSuffixes :: [a] -> [[a]]
generateSuffixes list = tail (foldl (\acc _ -> acc ++ [tail (last acc)]) [list] (tail list))


-- np. dla inputu [1,2,3,4,5] daje [[1], [1,2], [1,2,3], [1,2,3,4]]
generatePrefixes :: [a] -> [[a]]
generatePrefixes list = init (foldr (\_ acc -> init (head acc) : acc) [list] (tail list))


-- zwraca liste dzielnikow liczby podanej argumentem
divisors :: Int -> [Int]
divisors n = 1 : filter ((==0) . rem n) [2..n]


-- nieskonczona lista liczb pierwszych
primes :: [Int]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]


-- wszystkie przesuniecia cykliczne listy, np. dla [1,2,3] mamy [[1,2,3], [2,3,1], [3,1,2]]
cykl :: [a] -> [[a]]
cykl list = foldr (\_ acc -> (tail (head acc) ++ [head (head acc)]) : acc) [list] (tail list)


-- zwraca wszystkie napisy zlozone z liter "a" i "b" o dlugosci podanej argumentem
abStringsOfGivenLength :: Int -> [String]
abStringsOfGivenLength 1 = ["a", "b"]
abStringsOfGivenLength n = map ("a" ++) (abStringsOfGivenLength (n-1)) ++ map ("b" ++) (abStringsOfGivenLength (n-1))


-- dla podanego nizej typu...
type Point = (Double, Double)
-- ...oblicza odleglosc miedzy podanymi punktami
distance :: Point -> Point -> Double
distance p1 p2 = sqrt((fst p2 - fst p1)**2 + (snd p2 - snd p1)**2)


-- zwraca indeks na ktorym jest max tablicy
maxIndex :: [Int] -> Int
maxIndex = maxIndex2 0
maxIndex2 :: Int -> [Int] -> Int
maxIndex2 acc [] = -1
maxIndex2 acc (x:xs) = if x == maximum (x:xs) then acc else maxIndex2 (acc+1) xs


-- zwraca wszystkie podlisty listy podanej argumentem (zadanie pana Polanskiego)
podlisty :: [Int] -> [[Int]]
podlisty x = concat [podlistyOdI (drop i x) i | i <- [0..(length x - 1)]]
podlistyOdI :: [Int] -> Int -> [[Int]]
podlistyOdI x i = [take j x | j <- [1..(length x)]]

-- podlisty ale dokladnie dlugosci k
podlistyDlugosciK :: Int -> [Int] -> [[Int]]
podlistyDlugosciK k x = filter (\x -> length x == k) (podlisty x)


-- pierwszy argument to podstawa systemu liczbowego, drugi to lista cyfr liczby w tym systemie (od lewej)
-- funkcja zwraca wartosc liczby w systemie dziesietnym
doDziesietnego :: Int -> [Int] -> Int
doDziesietnego n l = fst (foldr (\x (a1, a2) -> (a1 + x*n^a2, a2 + 1)) (0, 0) l)