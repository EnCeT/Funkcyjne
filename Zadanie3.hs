-- f (8) = 4, f (4) = 2 i f (2) = 1.
-- Zadanie 3. Wieloszczet nierychliwy to struktura danych, która gromadzi dane liczbowe wskazanego typu z podziałem na segmenty. Wieloszczet nierychliwy pusty składa si˛e z jednego,
-- pustego segmentu; operacja wp zwraca taki wła´snie wieloszczet. Operacja dl doł ˛acza liczb˛e
-- do bie˙z ˛acego (najpó´zniej utworzonego) segmentu, za´s ts tworzy nowy segment i powoduje, ˙ze kolejne operacje dl działaj ˛a na owym nowym segmencie (chyba ˙ze bie˙z ˛acy segment
-- jest pusty; wówczas ts zwraca wieloszczet bez zmian). Operacja wmax podaje list˛e maksimów z kolejnych segmentów, poczynaj ˛ac od ostatnio doł ˛aczonego (je˙zeli bie˙z ˛acy segment
-- jest pusty, to jest pomijany, zatem lista maksimów jest wówczas o jeden krótsza ni˙z liczba segmentów). Podobnie wmin podaje list˛e minimów. Operacje wmax i wmin powinny by´c
-- wykonalne w czasie liniowym wzgl˛edem liczby segmentów, pozostałe operacje — w czasie
-- stałym.
-- Zdefiniowa´c typ Wnr a, słu˙z ˛acy do przechowywania elementów typu a w wieloszczecie nierychliwym, oraz nast˛epuj ˛ace funkcje, realizuj ˛ace opisane wy˙zej operacje z odpowiedni ˛a zło-
-- ˙zono´sci ˛a:
-- wp :: Wnr a
-- dl :: Num a => Wnr a -> a -> Wnr a
-- ts :: Wnr a -> Wnr a
-- wmax :: Wnr a -> [a]
-- wmin :: Wnr a -> [a]

--Zadanie.3
data Wnr a = Empty | Wnr [[a]]

wp :: (Eq a) => Wnr a -> [[a]]
wp Empty = []
wp (Wnr x) | head x == [] = wp (Wnr (tail x))
            | otherwise = x

dl :: Num a => Wnr a -> a -> Wnr a
dl (Wnr (x:xs)) n = (Wnr ((n:x):xs))

ts :: (Num a, Eq a) => Wnr a -> Wnr a
ts (Wnr x) | head x == [] = Wnr x
           | otherwise = (Wnr ([]:x))
             
            
wmax :: (Ord a) => Wnr a -> [a]
wmax (Wnr x)  = map (\a -> maximum a) x

wmin :: (Ord a) =>Wnr a -> [a]
wmin (Wnr x) = map (\a -> minimum a) x