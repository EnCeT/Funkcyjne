-- Zadanie 1. Dla liczby naturalnej w ka˙zdym kroku tworzymy liczb˛e b˛ed ˛ac ˛a sum ˛a jej
-- cyfr (w zapisie dziesi˛etnym) i post˛epujemy tak, a˙z dojdziemy do liczby jednocyfrowej.
-- Napisa´c funkcj˛e sevens :: Int → [Int], zwracaj ˛ac ˛a n pierwszych liczb, dla których 
-- powy˙zszy proces skonczy si˛e na liczbie 7. Na przykład sevens 4 = [7, 16, 25, 34]. Mo ´ ˙zna
-- te˙z zauwa˙zy´c, ˙ze do sevens 1000 nale˙zy np. liczba 8881, bo 8881 → 25 → 7.
--Zad.1
l:: Int -> Int -> Int
l n x | (n `div` x) >= 10 = l n (x*10) 
      | otherwise  = x

suma:: Int -> Int
suma n = if n >= 10 then y + suma (n `mod` x) else n
    where 
        x = l n 10
        y = n `div` x 

s:: Int -> Int
s n = if x > 15 then s x else x
    where 
        x = suma n

sevens:: Int -> [Int]
-- sevens n = take n (filter (\x -> (s x) == 7 ) [1..])

sevens n = take n [7,16..]

-- Zadanie 2. Palindrom nazywamy zbalansowanym, je˙zeli składa si˛e on wył ˛acznie z liter
-- a i b, i liczba liter a jest równa liczbie liter b. Napisa´c funkcj˛e bp :: Int → [String], która
-- dla danego n zwróci list˛e zbalansowanych palindromów o długo´sci n.
--Zad.2
podzbiory :: Int-> [String]
podzbiory 0  = [""]
podzbiory n = map ('a':) (podzbiory (n-1)) ++ map ('b':) (podzbiory (n-1))

parzyste :: [String] -> [String]
parzyste [] = []
parzyste (x:xs) | even (length x) = [x] ++ parzyste xs
                | otherwise = parzyste xs  

balance :: String -> Bool
balance x = (length (filter ('a' ==) x)) == (length (filter ('b' ==) x))


bp:: Int -> [String]
bp n = map (\x -> x ++ reverse x) (filter (\x -> balance x) p)
    where
        p = if even n then podzbiory (n `div` 2) else []

-- Zadanie 3. Miłorz ˛ab to struktura danych o funkcjonalno´sci przypominaj ˛acej list˛e, która
-- umo˙zliwia: dokładanie elementów na pocz ˛atek i na koniec w czasie stałym, odczytanie elementów po kolei w czasie liniowym wzgl˛edem liczby elementów oraz usuni˛ecie
-- wszystkich elementów doło˙zonych na pocz ˛atek („urwanie lewego listka”) i na koniec
-- („urwanie prawego listka”) w czasie stałym (nie liczymy czasu potrzebnego na zwolnienie pami˛eci). Stworzy´c typ Mb a, przechowuj ˛acy elementy typu a w miłorz˛ebie, i zdefiniowa´c funkcje:
-- dnp :: Mb a -> a -> Mb a
-- dnk :: Mb a -> a -> Mb a
-- mb2list :: Mb a -> [a]
-- ull :: Mb a -> Mb a
-- upl :: Mb a -> Mb a
-- Funkcje maj ˛a, odpowiednio, wstawia´c element na pocz ˛atek i na koniec miłorz˛ebu, zamienia´c miłorz ˛ab na list˛e (z elementami w odpowiedniej kolejno´sci, tzn. od pocz ˛atku
-- miłorz˛ebu do konca) oraz urywa ´ ´c lewy i prawy listek. Zło˙zono´s´c funkcji powinna by´c
-- taka, jak opisano wcze´sniej.

--Zad.3
data Mb a = Mb {poczatek :: [a], koniec :: [a]}

instance Show (Mb Integer)
    where 
        show (Mb p k) = "[" ++ show p ++ show k ++ "]"

dnp :: Mb a -> a -> Mb a
dnp (Mb p k) x = Mb (x:p) k

dnk :: Mb a -> a -> Mb a 
dnk (Mb p k) x = Mb p (x:k)

mb2list :: Mb a -> [a]
mb2list (Mb [p] k) = p:mb2list (Mb [] k)
mb2list (Mb (p:ps) k) = p:mb2list (Mb ps k)
mb2list (Mb [] [k]) = [k]
mb2list (Mb [] k) = (last k):mb2list (Mb [] (init k))

ull :: Mb a -> Mb a 
ull (Mb p k) = Mb [] k

upl :: Mb a -> Mb a
upl (Mb p k) = Mb p []