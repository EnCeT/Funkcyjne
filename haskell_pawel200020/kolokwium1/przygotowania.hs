
--  data Nat = Zero | Nast Nat

--  nat2int :: Nat -> Integer
--  nat2int Zero = 0
--  nat2int (Nast x) = 1 + nat2int x

--  instance Show Nat where
--    show x = show (nat2int x)

--  dodNat :: (Nat, Nat) -> Nat
--  dodNat (x, Zero) = x
--  dodNat (x, Nast y) = Nast (dodNat (x, y))

--  odejNat :: (Nat, Nat) -> Nat
--  odejNat (x, Zero) = x
--  odejNat (Nast x, Nast y) = odejNat (x, y)

--  mnozNat :: (Nat, Nat) -> Nat
--  mnozNat (x, Zero) = Zero
--  mnozNat (x, Nast y) = dodNat(x, mnozNat (x, y))

--  instance Eq Nat where
--      (==) Zero Zero  = True
--      (==) (Nast x) Zero = False
--      (==) Zero (Nast x) = False
--      (==) (Nast y) (Nast x) = (==) y x

-- iter :: (Integer, Integer -> Integer) -> (Integer -> Integer)
-- iter (0, f) x = x
-- iter (n, f) x = f (iter (n - 1, f) x)

-- f :: Integer ->Integer
-- f x = x

--  fibS :: Integer -> (Integer, Integer)
--  fibS 0 = (0, 1)
--  fibS n = (y, x + y)
--    where (x, y) = fibS (n-1)

--  fib :: Integer -> Integer
--  fib a = fst( fibS a)

rekdefp :: (a -> b -> b) -> b -> [a] -> b
rekdefp h e [] = e
rekdefp h e (x : xs) = h x (rekdefp h e xs)

dl :: [a] -> Integer
dl x = rekdefp dodaj1 0 x
  where
    dodaj1 x n = n + 1

f :: Int -> [Int]
f _ = filter (< 50) (map kw [0 ..])

kw x = x * x


listaKwadratow n = iterate (\x -> x * n) n


dupa :: Int -> Int -> [Int]
dupa n k = take k (listaKwadratow n)


doNJednykek n = takeWhile (\x->x<n*n) (listaKwadratow n)