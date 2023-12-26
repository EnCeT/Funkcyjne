fun :: Double -> Double -> Double
fun x y = ((x * x - 3 * y) / (x + y * y) + 10)

--plusik!!
fun2 :: Int -> Int
fun2 x = if x < 0 then (-1) * x * x else if x == 0 then -3 else 2 * x + 1

--dozory
fun3 :: Double -> Double
fun3 x
  | x < 0 = -x * x
  | x == 0 = -3
  | otherwise = 2 * x + 1

--ciąg fibonacciego
fib :: Int -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

--liczby stirlinga
stir :: Int -> Int -> Int
stir x y
  | x == 0 && y == 0 = 1
  | y == 0 = 0
  | x == 0 = 0
  | otherwise = stir (x - 1) (y - 1) + y * stir (x - 1) y
--funkcja zwracajaca funkcje - zadanie domowe
xd :: Int -> Int
xd x = x

sumSeq :: Int -> (Int -> Int) -> Int
sumSeq 1 f = f 1
sumSeq n f = sumSeq (n - 1) f + f n

bell :: Int -> Int -> (Int -> Int -> Int) -> Int
bell n 1 f = f 1 1
bell n k f = bell n (k - 1) f + f n k