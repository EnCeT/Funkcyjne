-- Zadanie 1. Niech type Point = (Double, Double). Napisa´c funkcj˛e
-- minDist :: [Point] -> (Point, Point, Double),
-- która zwróci par˛e punktów najbli˙zszych sobie na li´scie podanej jako argument, wraz
-- z odległo´sci ˛a mi˛edzy nimi. W przypadku wi˛ekszej liczby mo˙zliwych rozwi ˛azan nale ´ ˙zy
-- wybra´c dowolne; tak˙ze kolejno´s´c elementów w parze wynikowej nie ma znaczenia.
-- W rozwi ˛azaniu mo˙zna u˙zy´c faktu, i˙z (Double, Double) jest w klasie Eq. Przykładowo,
-- minDist [(1,5),(1,1),(10,1),(-4,1)] = ((1.0,1.0),(1.0,5.0),4.0)
--Zad.1
type Point = (Double, Double)

dist :: Point -> Point -> Double
dist (a,b) (c,d) = sqrt ((c-a)*(c-a) + (b-d)*(b-d))

mlist :: [Point] -> [(Point,Point)]
mlist xs = [(a,b) | a <- xs, b <- xs , a /= b]

distlist :: [(Point,Point)] -> [(Double,(Point,Point))]
distlist [(x,y)] = [(dist x y,(x,y))]
distlist ((x,y):xs) = (dist x y,(x,y)):distlist(xs)

minlist :: [(Double,(Point,Point))] -> [Double]
minlist [(a,(b,c))] = [a]
minlist ((a,(b,c)):xs) = a:minlist xs 

minDist :: [Point] -> (Point, Point, Double)
minDist list = (b,c,a)
        where
                dl = distlist (mlist list)
                min = minlist dl
                (a::Double,(b::Point,c::Point)) = head (filter (\ x -> minimum min == fst x ) dl)

-- Zadanie 2. Rozwa˙zmy nast˛epuj ˛ac ˛a definicj˛e drzewa:
-- data Tree a = Empty | Node a (Tree a) (Tree a).
-- Napisa´c funkcj˛e
-- findPath :: Eq a => a -> Tree a -> [a],
-- która zwraca ´scie˙zk˛e (w postaci listy) od korzenia drzewa do elementu podanego jako
-- pierwszy argument. Prosz˛e zało˙zy´c, ˙ze w drzewie nie ma powtarzaj ˛acych si˛e elementów.
-- Na przykład dla
-- t=Node 10 (Node 5 (Node 4 Empty Empty) (Node 6 Empty Empty)) (Node 20
-- Empty Empty)
-- mamy findPath 6 t = [10,5,6] oraz findPath 7 t = [].


--Zad.2
data Tree a = Empty | Node a (Tree a) (Tree a)

path :: Eq a => a -> [a] -> Tree a -> [a]
path _ _ Empty = []
path x stack (Node val left right) 
    | x == val = val:stack
    | otherwise = path x (val:stack) left ++ path x (val:stack) right

findPath :: Eq a => a -> Tree a -> [a]
findPath x tree = reverse (path x [] tree)

myTree :: Tree Int
myTree = Node 10 (Node 5 (Node 4 (Node 22 Empty Empty) Empty) (Node 6 Empty Empty)) (Node 20 (Node 30 Empty Empty) (Node 40 Empty Empty))

-- Zadanie 3. W permutacji liczb od 1 do n pozycj˛e k nazywamy zamykaj ˛ac ˛a, je˙zeli w´sród
-- pozycji od 1 do k znajduj ˛a si˛e wszystkie liczby od 1 do k. Napisa´c funkcj˛e
-- cp :: [Integer] -> [Integer],
-- która dla permutacji podanej jako lista wyliczy wszystkie jej pozycje zamykaj ˛ace. Przykładowo, cp [1,2,3] = [1,2,3], za´s cp [3,2,1] = [3].

--Zad.3
c :: Integer -> [Integer] -> Bool
c 1 x = 1 `elem` x 
c n x = if n `elem` x then c (n-1) x else  False

cp::[Integer] -> [Integer]
cp [x] = if x == 1  then [x] else []
cp x = if c n x then  cp (take (m - 1) x) ++ [n] else cp (take (m - 1) x )
        where
             n = fromIntegral (length x) 
             m = length x