--złożenia funkcji 

iter :: Integer -> (a -> a) -> (a -> a)
iter 0 _ = id
iter n f = f . iter (n-1) f

pot :: Num a => Integer -> a -> a
pot 0 _ = 1
pot n x = if even n then p else x*p
    where p = pot (n `div` 2) (x*x)

iter2 :: Num a => Integer -> (a -> a) -> (a -> a)
iter2 0 _ = id
iter2 n x = if even n then p else x.p
    where p = iter2 (n `div` 2) (x.x)

data Nat = Zero | Nast Nat

odej :: Nat -> Nat -> Nat
odej Zero x = Zero
odej x Zero = x
odej (Nast x) (Nast y) = odej x y 


rekdef :: (a -> a) -> a -> (Integer -> a)
rekdef h c 0 = c
rekdef h c n = h (rekdef h c (n - 1))

-- silnia = scd.rekdef (\(n,acc) -> (n + 1, (n + 1) * acc))

czypusta :: Eq a => [a] -> Bool
czypusta x = (x == [])

pusta :: [a] -> Bool
pusta [] = True
pusta _ = False

ostatni :: [a] -> a
ostatni [x] = x
ostatni (x:xs) = ostatni xs
ostatni [] = error "Lista pusta brak ostatniego elementu"

--wzorce dla par: (x,x) , (x,_) , (_,_) _to dowolna rzecz której nie używamy w danym przypadku
--wzorce dla list: [] [x] (x:xs) (x1:x2:xs) itp.
--wzorce dla dowolnego typu: x _ (konkretna wartość)
