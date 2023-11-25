--materiały z labów nr 2 polana

-- funkcja pokzująca dodawania z deklaracją klasy zmiennej a w tym przypadku jako  Num oraz Show
napiszDodawanie :: (Num a, Show a) => a -> a -> String
napiszDodawanie x y = "Liczba " ++ show x ++ " powiekszona o " ++ show y ++
    " daje nam " ++ show (x+y)

-- funkcja tworząca listę kadratów pierwszych 10 liczb
kwadraty :: [Int]
kwadraty = [x * x | x <- [1..10]]

-- funkcja tworząca listę kwadratów liczb parzystych z przedziału <1..10>
kwadraty2 :: [Int]
kwadraty2 = [x * x | x <- [1..10], even x]

--funkcje które działają na listach

-- funkcja tworzy nieskończoną listę z podanym argumentem
powtarzanie :: [Int]
powtarzanie = repeat 1

--analogicznie z listą 
cyklicznie :: [Int]
cyklicznie = cycle [6,9]

--sprawdza czy podany element jest w podanej liście
sprelem :: Bool
--sprelem = elem 1 [1, 2]
sprelem = 1 `elem` [1, 2, 3]

--funkcja filter, zwraca listę z elementami spełniającymi warunek
sqlquery :: Int -> [Int] -> [Int]
sqlquery x list = filter (>x) list 

--
lista :: [Int] -> String
lista [] = "Pusta"
lista (x : []) = "Zawiera " ++ show x
lista (x:y:[]) = "Zawiera 2 elementy " ++ show x ++ " oraz " ++ show y
lista (x:xs) = "Lista zaczyna sie od " ++ show x ++ "reszta to " ++ show xs
