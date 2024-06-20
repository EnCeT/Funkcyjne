import qualified Distribution.SPDX as Zad
-- Napisać bezpunktowo funkcje równoważne (w sensie zwracanych
-- wartości) poniższym:
-- f list = filter (\x->x>5)) list
-- g list = map (\x->x/5) list
-- Przedstawić kolejne kroki przekształceń od postaci punktowej do
-- bezpunktowej.
-- Zad.1
f :: [Int] -> [Int]
f = filter (>5)

g :: [Float] -> [Float]
g = map (/5)

-- Napisać bezpunktowo funkcję o sygnaturze
-- nonZero :: [Int] -> Int
-- obliczającej liczbę niezerowych elementów na liście. Przedstawić
-- kolejne kroki przekształceń od postaci punktowej do bezpunktowej.
-- Zad.2
nonZero :: [Int] -> Int
nonZero = length . filter (/=0)

-- nonZero list = length (filter (/=0) list)
-- nonZero list = length $ filter (/=0) list
-- nonZero list = length . filter (/=0) list
-- nonZero = length . filter (/=0)

-- Napisać bezpunktowo funkcję równoważną (w sensie zwracanych
-- wartości) poniższej:
-- m x list = map (\y->y*x) list
-- Przedstawić kolejne kroki przekształceń od postaci punktowej do
-- bezpunktowej.
-- Zad.3
m :: Int -> [Int] -> [Int]
m = map . (*)

-- m x list = map (\y->y*x) list
-- m x = map (\y->y*x)
-- m x = map (\y->(*) y x)
-- m x = map ((*) x)
-- m = map . (*)

-- Napisać bezpunktowo funkcję równoważną (w sensie zwracanych
-- wartości) poniższej:
-- d :: [Double] -> Double -> [Double]
-- d list x = map (\y->y/x) list
-- Przedstawić kolejne kroki przekształceń od postaci punktowej do
-- bezpunktowej.
-- Zad.4
d :: [Double] -> Double -> [Double]
d = flip $ map . (/)

-- d list x = map (\y->y/x) list
-- d list x =  flip (map . (/)) list x 
-- d list = flip (map . (/)) list
-- d = flip (map . (/))

-- Napisać bezpunktowo funkcję równoważną (w sensie zwracanych
-- wartości) poniższej:
-- wiekszeOd lista a = [x | x<-lista,x>a]
-- Przedstawić kolejne kroki przekształceń od postaci punktowej do
-- bezpunktowej
--Zad.5
wiekszeOd :: [Int] -> Int -> [Int]
wiekszeOd = flip (filter . (<))

-- wiekszeOd lista a = [x | x<-lista,x>a]
-- wiekszeOd lista a = filter (\x->x>a) lista
-- wiekszeOd lista a = filter (>a) lista
-- wiekszeOd lista a = flip (filter . (<)) lista a
-- wiekszeOd lista = flip (filter . (<)) lista
-- wiekszeOd = flip (filter . (<))
