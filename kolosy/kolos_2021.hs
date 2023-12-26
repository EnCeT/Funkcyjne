-- Zadanie 1. Rozpatrzmy nast˛epuj ˛acy typ reprezentuj ˛acy graf skierowany:
-- type DirectedGraph = ([Int], Int -> Int -> Bool).
-- Pierwszy element pary to zbiór wierzchołków V, drugi to funkcja f taka, ˙ze dla dowolnych
-- x, y ∈ V mamy, ˙ze f x y = True ⇐⇒ x y jest (skierowan ˛a) kraw˛edzi ˛a z x do y. Napisa´c
-- funkcj˛e
-- atDistance :: DirectedGraph -> Int -> Int -> [Int],
-- która dla wywołania atDistance g d v zwraca list˛e (by´c mo˙ze z powtórzeniami) wszystkich
-- wierzchołków, które w grafie g s ˛a osi ˛agalne z v drog ˛a długo´sci dokładnie d. 
-- Dla przypomnienia: w drodze wierzchołki mog ˛a si˛e powtarza´c, za´s długo´s´c drogi to liczba jej kraw˛edzi.

-- zad 1
atDistance :: ([a], a->a->Bool) -> Int -> a -> [a]
atDistance g 0 v = [v]
atDistance g d v = [y | x <- fst g, snd g v x, y <- atDistance g (d-1) x]

-- Zadanie 2. Napisa´c funkcj˛e o sygnaturze
-- wIluListach :: Int -> [[Int]] -> [Int],
-- która dla liczby całkowitej dodatniej n oraz listy [l1
-- , l2
-- , ..., lk
-- ] list liczb naturalnych z przedziału {1, ..., n} sprawdza, w jak wielu listach l1
-- , l2
-- , ..., lk wyst˛epuj ˛a kolejne liczby 1, ..., n. Inaczej
-- mówi ˛ac, funkcja ma zwróci´c list˛e, której pierwszym elementem jest liczba list, w których wyst˛epuje liczba 1, 
-- drugim — liczba list, w których wyst˛epuje 2 itd. Przykładowo:
-- wIluListach 7 [[1,2,3],[3,4,5]] ma zwróci´c [1,1,2,1,1,0,0], poniewa˙z liczba 1
-- wyst˛epuje w jednej li´scie, podobnie jak 2, 4 i 5; liczba 3 wyst˛epuje w dwóch listach, a 6 i 7 —
-- w ˙zadnej,
-- wIluListach 7 [[1,2,3],[3,4,5],[5,6,1],[1,7,4],[3,7,6],[2,7,5],[2,4,6]]
-- ma zwróc

-- zad 2
fun6 :: Int -> [[Int]] -> Int
fun6 x lists = sum (map (fromEnum . elem x) lists)
fun7 :: Int -> [[Int]] -> [Int]
fun7 x lists = [fun6 y lists | y<-[1..x]]

-- Zadanie 3. Wierzba rosochata to struktura danych, która pozwala na: doł ˛aczenie gał˛ezi (tj. listy
-- elementów), usuni˛ecie ostatnio doł ˛aczonej gał˛ezi, doł ˛aczenie elementu do ostatnio doł ˛aczonej
-- gał˛ezi, usuni˛ecie ostatnio doł ˛aczonego elementu (pod warunkiem, ˙ze od ostatniego doł ˛aczenia
-- elementu nie były wykonywane operacje na gał˛eziach), podanie liczby gał˛ezi oraz zamian˛e
-- całej struktury na list˛e w taki sposób, ˙ze ostatnio doł ˛aczona gał ˛a´z pojawia si˛e w li´scie przed
-- pozostałymi gał˛eziami. Zamiana na list˛e powinna by´c wykonalna w czasie liniowym wzgl˛edem
-- ł ˛acznej liczby elementów, natomiast pozostałe operacje — w czasie stałym. Zdefiniowa´c typ
-- Wr a, słu˙z ˛acy do przechowywania elementów typu a w wierzbie rosochatej, oraz nast˛epuj ˛ace
-- funkcje, realizuj ˛ace opisane wy˙zej operacje z odpowiedni ˛a zło˙zono´sci ˛a:
-- dg :: Wr a -> [a] -> Wr a
-- ug :: Wr a -> Wr a
-- de :: Wr a -> a -> Wr a
-- ue :: Wr a -> Wr a
-- lg :: Wr a -> Integer
-- wr2l :: Wr a -> [a]

-- zad 3
data Wr a = Wr Int Bool [[a]]

dg :: Wr a -> [a] -> Wr a
dg (Wr n _ gs) xs = Wr (n + 1) True (xs:gs) 

ug :: Wr a -> Wr a
ug (Wr n _ (g:gs)) = Wr (n - 1) False gs
ug (Wr n w []) = Wr n w []

de :: Wr a -> a -> Wr a
de (Wr n False (g:gs)) _ = Wr n False (g:gs)
de (Wr n True (g:gs)) x = Wr n True ((x:g):gs)

ue :: Wr a -> Wr a
ue (Wr n False gs) = Wr n False gs
ue (Wr n True ((x:g):gs)) = Wr n False (g:gs)

lg :: Wr a -> Integer
lg (Wr n _ _) = toInteger n

wr2l :: Wr a -> [a]
wr2l (Wr _ _ gs) = concat gs