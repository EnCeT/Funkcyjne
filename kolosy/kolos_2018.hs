-- zad 1
an :: Int -> Int
bn :: Int -> Int

an 0 = 1
an n = (n-1) * bn (n-1) - 3 * an (n - 1)

bn 0 = 1
bn n = 3 * bn (n-1) + (n - 1)*(n - 1) * an (n - 1) - (n - 1)*(n - 1)

seqHelper :: Int -> Int -> Int
seqHelper m n
    | sum (take n (map an [0..])) >= m = n - 1
    | otherwise = seqHelper m (n+1)

seqIndex :: Int -> Int
seqIndex m = seqHelper m 1

-- zad 2
data Expr a = Value a
    | Add (Expr a) (Expr a)
    | Mul (Expr a) (Expr a)
    | Sub (Expr a) (Expr a)
    | P

eq :: (Eq a) => Expr a -> Expr a -> Bool
eq _ P = True
eq P _ = True
eq (Value a) (Value b) = a == b
eq (Add a b) (Add c d) = eq a c && eq b d
eq (Mul a b) (Mul c d) = eq a c && eq b d
eq (Sub a b) (Sub c d) = eq a c && eq b d
eq _ _ = False

-- zad 3
cykl :: [a] -> [[a]]
cykl xs = foldl (\acc _ -> (last (head acc):init (head acc)):acc) [xs] (init xs)