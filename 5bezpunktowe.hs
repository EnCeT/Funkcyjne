-- Bezpunktowe gówno
doubleList :: [Int] -> [Int]
-- doubleList list = map (*2) list -- zwykła funkcja

doubleList = map (*2) -- bezpunktowe

-- operator $ - aplikacja funkcji
-- użyty zaraz za wywołanie funkcji zastępuje nawiasy
-- i w sumie tylko do tego służy do redukcji nawiasowania
f :: Float -> Float -> Float -> Float
f x y z = sqrt $ x + y + z -- to to samo co sqrt (x + y + z)

-- operator . - kompozycja funkcji
-- (.) :: (b -> c) -> (a -> b) -> a -> c o to jak wygląda deklaracja tego operatora

-- przykład użycia
-- (f . g) x = f (g x)
-- (f . g . h) x = f (g (h x))

-- f x y = 3*x+y
-- f x y = (+) (3*x) y
-- f x = (+) (3*x)
-- f x = (+) ((*) 3 x)
-- f x = ((+).(*) 3) x
-- f = (+).(*) 3

-- filp zamienia kolejność argumentów funkcji
-- flip :: (a -> b -> c) -> b -> a -> c
-- flip f x y = f y x
-- f x y = 3*y/x
-- f x y = (/) (3*y) x
-- f x y = flip (/) x (3*y)
-- f x = (flip (/) x).(3*)
-- f x = (.)(flip (/) x)(3*)
-- f x = flip (.) (3*) (flip (/) x)
-- f = (flip (.) (3*)).(flip (/))

-- funkcja curryfikująca
-- curry :: ((a,b)->c)->a->b->c
-- (curry f) x y = f (x,y)
-- uncurry :: (a->b->c)-> ((a,b)->c)
-- (uncurry f)(x,y) = f x y
-- Przykładowo, zamiast
-- map (\(x,y) = x*y) [(1,2),(3,4),(5,6)]
-- Możemy użyć
-- map (uncurry (*)) [(1,2),(3,4),(5,6)]