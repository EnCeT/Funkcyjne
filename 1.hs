-- :: Integer -> Integer

-- (/) 1.0 3.0 dzielenie 1 przez 3
-- (1.0/3.0) też dzeilenie
-- (/3.0) 1.0 takie samo dzielenie
-- :t (/) 3.0 zapytanie o typ danych w funkcji
-- 
-- div 7 2 dzielnie bez reszty 
-- 7 'div' 2 też dzielenie

(##) :: Integer -> Integer -> Integer
x ## y = 2*x + 3*y
-- definiowanie własnego operatora

s :: Integer -> Integer
s 0 = 1
s n = if n > 0 then n * s (n-1)
      else error  "Zły argument funkcji"
-- silnia

f :: Integer -> Integer
f 0 = 1
f 1 = 1
f n = f(n-1) + f(n - 2)
-- fibonacciego O(n)2^n

-- funkcja pomocnicza
fp :: Integer -> Integer -> Integer -> Integer
fp x y 0 = y
fp x y n = fp y (x + y) (n - 1)
-- nieco lepszy fibonacciego
--fibb :: Int -> Int
--fibb n = fp 0 1 n



para x = (x, x)

kw x = x * x

-- potęgowanie szybkie 
p :: Integer -> Integer -> Integer
p 0 x = 1
p n x = if even n then w else x * w 
        where w = p (n `div` 2) (x*x)

type M = (Integer, Integer, Integer, Integer)
mnM :: M -> M -> M
mnM (x, y, z, w) (x', y', z', w') = ((x*x' + y*z'), (x*y' + y *w'), (z*x' + w*y'), (z*y' + w*w'))

potM :: Integer -> M -> M
potM 0 _ = (1,0,0,1)
potM n (x, y, z, w) = if even n then (a, b, c, d) else mnM (x, y, z, w) (a, b, c, d)
                      where (a, b, c, d) = potM (n `div` 2) (mnM (x, y, z, w) (x, y, z, w))
--potM n (x, y, z, w) = potM (n-1) ( mnM (x,y,z,w) (x,y,z,w))
--do poprawy, napisać jak potęgowanie liczb

mnk :: M -> Integer
mnk (_, y, _, _) = y

fibb :: Integer -> Integer
fibb n = mnk $ potM n (0, 1, 1, 1)




