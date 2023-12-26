
-- take k list
-- @desc: Takes first k elements from list 
-- @example: 
--          in: take 3 [1,2,3,4,5]
--          out: [1,2,3]

---------------------------------------------------------------

-- drop k list
-- @desc: drops (deletes) first k elems from list
-- @example: 
--          in: drop 3 [1,2,3,4,5,6]
--          out: [4,5,6]

---------------------------------------------------------------

-- maximum list
-- @desc: returns greates elem from list
-- @example: 
--          in: maximum [3,22,0,123123,-32,13,27]
--          out: 123123

---------------------------------------------------------------

-- minimum 
-- (jak wyzej)

---------------------------------------------------------------

-- product list
-- @desc: multiplies elemens in list
-- @example: 
--          in: product [1,2,3,4,5]
--          out: 120

---------------------------------------------------------------

-- replicate k obj
-- @desc: tworzy tablice obiektow (obj) dugosci k
-- @example:
--          in: replicate 3 ("tuple", 23123)
--          out: [("tuple",23123),("tuple",23123),("tuple",23123)]

---------------------------------------------------------------

-- cycle list
-- @desc: makes list a cycle (cycled list is infinite)
-- @example: 
--          in: (cycle [0,1,2,3,4]) !! 125
--          out: 0

---------------------------------------------------------------

-- repeat obj
-- @desc: powtarza obiekt w nieskonczonosc
-- @example: 
--          in: (repeat 2) !! 999 
--          out: 2

---------------------------------------------------------------

-- zipWith funcion list1 list2
-- @desc: odpala funkcje na obu listach na odpowiadajcyh sobie indexach
-- @example:
--          in: zipWith (*) [1, 2, 0] [7, 11, 13]
--          out: [7,22,0]
-- @NOTE: function musi byc (a->a)->a   (dla list typu a)
-- @NOTE2: Istnieje wersja dla 3 tablic (dziala analogicznie), nazywa sie zipWith3

---------------------------------------------------------------

-- takeWhile function list
-- @desc: zwraca elementy dopoki spelniaja warunek (zdefiniowany w "function")
-- @example:
--          in: takeWhile (\x->x <10) [1..999]
--          out: [1,2,3,4,5,6,7,8,9]
-- @NOTE: function musi byc (a->Boolean)

---------------------------------------------------------------

-- iterate function startValue
-- @desc: tworzy nieskonczona liste zaczynajaca sie na startValue, 
--        kolejne wartosci obliczane za pomoca podanej funkcji
-- @example:
--          in: take 10 (iterate (2*) 3)
--          out: [3,6,12,24,48,96,192,384,768,1536]

---------------------------------------------------------------

-- foldl function startValue list
-- @desc: odpala funkcje na elementach list od lewej strony zaczynajac od "function startValue x1"
-- @example: 
--          in: foldl (*) 1 [1,2,3,4,5]
--          out: 120 (((((1*1)*2)*3)*4)*5)
--          in: foldl (/) 8 [1,2,3]
--          out: 1.333333333    

---------------------------------------------------------------

-- foldl function startValue list
-- @desc: odpala funkcje na elementach list od prawej strony zaczynajac od "function startValue x1"
-- @example: 
--          in: foldr (/) 8 [1,2,3]
--          out: 0.1875

---------------------------------------------------------------

-- zip list1 list2
-- @desc: laczy listy w tuplr
-- @example:
--          in: zip [1,2,3,4,5] ["marek", "tomek"]
--          out: [(1,"marek"),(2,"tomek")]
-- @NOTE: laczenie konczy sie na krotszej liscie
-- @NOTE2: Istnieje wersja dla 3 tablic (dziala analogicznie), nazywa sie zip3

---------------------------------------------------------------

-- head list / last list
-- @desc: zwraca 1. element listy  / zwraca ostatni element listy
-- @NOTE: nie dziala na tuplach

---------------------------------------------------------------

-- tail / init
-- @desc:zwraca wszyscko poza 1. elementem listy / poza ostatnim elementem listy

---------------------------------------------------------------

-- length list
-- @desc: zwraca dlugosc listy
-- @NOTE: dla tupla zwroci 1

---------------------------------------------------------------

-- elem k list
-- @desc: sprawdza czy element k jest w liscie
-- @example: 
--          in: elem 0 [1,23,4,5]
--          out: False

---------------------------------------------------------------

-- and/or lista
-- @desc: zwraca wynik opecaji and/or na liscie
-- @example:
--          in: and [True, False, True]
--          out: False
-- @NOTE: dziala tylko dla BOOLi

---------------------------------------------------------------

-- sum list
-- @desc: zwraca sume elemntow listy
-- @example:
--          in: sum [5,10,5,7]
--          out: 27

---------------------------------------------------------------

-- concat [list1, list2...listN]
-- @desc: laczy listy (od lista1 do listaN) w jedna liste, kolejnosc danych sie nie zmienia
-- @example:
--          in: concat [[1,3,5,7], [2,4,6,8]]
--          out: [1,3,5,7,2,4,6,8]

---------------------------------------------------------------

-- concatMap function list
-- @desc: wykonuje dzialanie na kazdym elemencie listy, na koncu kontatenuje wynik
-- @example: 
--          in: concatMap (\x -> [(x,x+2,x/2)]) [1,3,5] 
--          out: [(1.0,3.0,0.5),(3.0,5.0,1.5),(5.0,7.0,2.5)]
-- @NOTE: Ze zwyklym 'map' dostalibysmyu liste list

---------------------------------------------------------------

-- null
-- @desc: returns True if a list is empty, otherwise False 
-- @example:
--          in: null [] 
--          out: True
--          in: null "Hello" 
--          out: False

---------------------------------------------------------------

-- dropWhile function list
-- @desc: Usuwa elementy dopoki spelniony jest warunek
-- @example:
--          in: dropWhile (\x->x<10) [1,2,3,4,11,55,0,-1232,8]
--          out: [11,55,0,-1232,8]
-- @NOTE: mimo ze 0, -1232 i 8 spelniaja warunek, 
--        to nie zostaly usuniete bo byly po 11, ktora nie spelnia

---------------------------------------------------------------

-- flip function x y
-- @desc: zamienia kolejnosc argumety funkcji
-- @example:
--          in: flip (>) 3 5
--          out: True
-- @NOTE: dziala tylko dla 2 argumentow

---------------------------------------------------------------

-- until function1 function2 startValue
-- @desc: Powtarza wywolywanie function2 na startValue dopoki warunek 
--        zdefiniowany function1 nie jest spelniony
-- @example:
--          in: until (> 100) (*2) 1 
--          out: 128
--          in: until (> 100) (*2) 1000
--          out: 1000

---------------------------------------------------------------

-- div
-- @desc: dzieli liczby, zwraca czesc calkowita
-- @example:
--          in: div 5 3
--          out: 1
-- @NOTE: nie dziala dl aliczb ujemnych

---------------------------------------------------------------

-- max number1 number 2 / min number1 number 2 
-- @desc: zwraca liczbe wieksza/mniejsza sposrod 2
-- @NOTE: dziala tylko dla 2 argumetnow

---------------------------------------------------------------

-- toInteger number
-- @desc: konwertuje liczbe do Integera
-- @example: 
--          in: toInteger (truncate  5.4)
--          out: 5      -- ta 5 jest Integerem, inaczej bylaby "Num"
--          in: toInteger 8
--          out: 8      -- ta 8 jest Integerem, inaczej bylaby "Num"
-- @NOTE: bez truncate w tym przypadku nie zadziala

---------------------------------------------------------------

-- unzip listWithTuples
-- @example:
--          in: unzip [(1,"a"),(2,"b"),(3,"c")] 
--          out: ([1,2,3],["a","b","c"])
-- @NOTE: unzip3 dziala analogicznie dla potrojnych tupli [(a,b,c)] -> ([a],[b],[c])

----------------------------------------------------------------------------------------

-- INNE
-- 2 : [4,8,16]     -- dodanie elementu na poczatek listy
-- [1,2] ++ [5,4]  -- laczenie listy
-- 7 `elem` list    -- Uzycie ` ` aby arguemtny zapisac w takiej formie
-- ("lalal", 2312)  -- tuple są
-- fst              -- 1st dla tupli
-- snd              -- 2nd element 
-- [0..1000]        -- Lista od 0 do 1000 (liczby calkowite)
-- [2,4..20]        -- liczby parzyste do 20

-- dozory (guards) & 'do'
-- f x | x < 0 = -x * x | x>0=x*x | otherwise= do
--     let temp = 3
--     temp + x

----------------------------------------------------------------------------------------

-- Filtrowanie podczas tworzenia listy
-- in:  [x | x <- [1,2..], x `mod` 6 == 1, x `mod` 7 == 4, x `mod` 8 == 3]
-- out: [67,235,403,571,739]


-- in:  [x * 3 | x <- [1..10], x*3 <50]		--filtr (tylko mnijsze od 50)
-- out: [3,6,9,12,15,18,21,24,27,30]


-- in:  [x * 2 | x <- [1..10]]		--	dla kazdego X z przedzialu 0-10 wslozyc do listy x*2
-- out: [x * 2 | x <- [1..10]]

