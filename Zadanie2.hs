-- Zadanie 2. Rozwa˙zmy funkcj˛e f okre´slon ˛a na liczbach naturalnych dodatnich, która zwraca
-- n/2 dla liczby n parzystej oraz 3n + 1 dla liczby n nieparzystej. Hipoteza Collatza mówi, ˙ze
-- je´sli zaczniemy od dowolnej liczby naturalnej, to wielokrotnie wywołuj ˛ac na niej f zawsze
-- otrzymamy w koncu 1. Zaimplementowa ´ ´c funkcj˛e o sygnaturze
-- collatz :: Integer -> [Integer],
-- która wywołana na n zawraca list˛e zawieraj ˛ac ˛a minimaln ˛a liczb˛e wywołan funkcji ´ f dla 
-- kolejnych liczb naturalnych od 1 do n, po której zostanie osi ˛agni˛ety wynik 1. Wywołanie nigdy
-- si˛e nie konczy, je ´ ´sli w liczonym zakresie istnieje kontrprzykład. Przykładowo, collatz 5
-- powinno zwróci´c [0, 1, 7, 2, 5], poniewa˙z f (3) = 10, f (10) = 5, f (5) = 16, f (16) = 8,
-- f (8) = 4, f (4) = 2 i f (2) = 1.
--Zadanie.2
col :: Int -> Int -> Int
col 1 x = x
col n x | even n = col (n `div` 2) (x + 1)
        | odd n = col ((3*n) + 1) (x + 1)

collatz :: Int -> [Int]
collatz n = map (\x -> col x 0) [1..n]