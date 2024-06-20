-- Zadanie 2. Jedn ˛a z mo˙zliwych definicji liczby Eulera jest suma szeregu
-- X∞
-- i=0
-- 1
-- i!
-- .
-- Napisa´c bezpunktowo funkcj˛e o sygnaturze
-- liczbaEulera :: Int -> Double,
-- przybli˙zaj ˛ac ˛a liczb˛e Eulera poprzez sum˛e
-- Xn
-- i=0
-- 1
-- i!
-- dla nieujemnego argumentu n. W rozwi ˛azaniu nale˙zy w istotny sposób u˙zy´c funkcji foldl/foldr.
-- Uwaga: mo˙zna zdefiniowa´c pomocnicze funkcje, je´sli równie˙z s ˛a napisane bezpunktowo. List˛e
-- [1..n] mo˙zna otrzyma´c poprzez take n [1..].
silnia :: Int -> Double
silnia = foldl (*) 1 . (flip take [1..]) 

lista :: Int -> [Int]
lista = flip take [1..]

liczbaEulera :: Int -> Double
-- liczbaEulera n = foldl (+) 0 (map (\x -> 1 / silnia x) (lista n))
-- liczbaEulera n = foldl (+) 0 . map (\x -> 1 / silnia x) . lista $ n
-- liczbaEulera = foldl (+) 0 . map (\x -> 1 / silnia x) . lista
liczbaEulera = foldl (+) 0 . map ((1 /) . silnia) . lista


