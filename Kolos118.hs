-- Zadanie 1. Rozwa˙zmy ci ˛agi (an),(bn), które spełniaj ˛a nast˛epuj ˛ac ˛a zale˙zno´s´c rekurencyjn ˛a
-- an = (n − 1)bn−1 − 3an−1
-- bn = 3bn−1 + (n − 1)2
-- an−1 − (n − 1)2
-- Dodatkowo wiemy, ˙ze a0 = b0 = 1. Napisa´c funkcj˛e seqIndex m, która zwraca najmniejsze k takie, ˙ze a0 + a1 + . . . + ak ­ m. Na przykład seqIndex 100 = 4, seqIndex 1000000
-- = 8.
-- Zad.1
an :: Int -> Int
an 0 = 1
an n = (n - 1)*(bn (n-1)) - 3*(an (n-1))

bn :: Int -> Int
bn 0 = 1 
bn n = 3 * (bn (n - 1))+ (n - 1)*(n - 1) * (an (n-1)) - (n - 1 )*(n - 1)

suma :: Int -> Int -> Int
suma n iter | n < sum (take iter (map (an) [1,2..])) = iter
           | otherwise = suma n (iter + 1)

seqIndex :: Int -> Int
seqIndex n = suma n 1

-- Zadanie 2. Rozwa˙zmy typ danych
-- data Expr a = Value a
-- | Add (Expr a) (Expr a)
-- | Mul (Expr a) (Expr a)
-- | Sub (Expr a) (Expr a)
-- | P
-- przechowuj ˛acy cz˛e´sciowe wyra˙zenia, tzn. wyra˙zenia, które zawieraj ˛a operacje dodawania (Add), mno˙zenia (Mul) i odejmowania (Sub) oraz warto´s´c P, która oznacza, i˙z konkretny argument nie jest jeszcze znany. Argumenty do operacji arytmetycznych przechowujemy za pomoc ˛a Value. Napisa´c funkcj˛e eq :: (Eq a) → Expr a → Expr a → Bool,
-- która zwraca True, je´sli wyra˙zenia s ˛a takie same, i False w przeciwnym wypadku. Przyjmujemy, ˙ze dwa wyra˙zenia s ˛a takie same, je´sli jedno mo˙zna otrzyma´c z drugiego przez
-- zamiany P na dowolne inne wyra˙zenia. Na przykład
-- eq (Add (Value 1) (Value 2)) (Add (Value 1) (Value 3)) = False
-- eq (Add (Value 1) (Value 2)) P = True

--Zad.2
data Expr a = Value a
            | Add (Expr a) (Expr a)
            | Mul (Expr a) (Expr a)
            | Sub (Expr a) (Expr a)
            | P

res :: (Num a) => Expr a -> a
res (Value x) = x
res (Mul x y) = (res x) * res y
res (Sub x y) = (res x) - res y
res (Add x y) = (res x) + res y


eq:: (Eq a,Num a) => Expr a -> Expr a -> Bool
eq P _ = True
eq _ P = True
eq a b = (res a) == (res b)

-- Zadanie 3. Napisa´c funkcj˛e cykl, która dla podanej niepustej listy zwraca list˛e jej wszystkich przesuni˛e´c cyklicznych, w dowolnej kolejno´sci. Poda´c najogólniejsz ˛a mo˙zliw ˛a sygnatur˛e. Funkcja ma w nietrywialny sposób korzysta´c z foldl lub foldr, przy czym fold
-- musi stanowi´c najbardziej zewn˛etrzn ˛a cz˛e´s´c definicji, np. cykl ` = foldl ... Przykładowo, wywołanie cykl [1,2,3] powinno zwróci´c [[1,2,3], [2,3,1], [3,1,2]] lub dowoln ˛a
-- permutacj˛e takiej listy.

--Zad.3
cykl :: [a] -> [[a]]
cykl list =
    foldl
      (\acc el -> acc ++ [drop 1 (last acc) ++ [el]])
      [list]
      (init list)