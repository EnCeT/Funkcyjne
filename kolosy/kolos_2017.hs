-- zad 1
dzielniki :: Integer -> [Integer]
dzielniki x = [y | y <- [1..x], x `mod` y == 0]

rd :: Integer -> [Integer]
rd n
    | n == 1 = [k | k <- [1..], sum (dzielniki k) == n+k]
    | otherwise = helper n 1
        where helper m k
                | m * m == k = []
                | sum (dzielniki k) == m + k = k : helper m (k + 1)
                | otherwise = helper m (k + 1)

-- zad 2
repl :: Eq a => [a] -> [(a, a)] -> [a]
repl xs ys = [foldl (\acc y -> if fst y == x then snd y else acc) x ys | x <- xs]

-- zad 3
data Tree a = Empty | Node a (Tree a) (Tree a)
data Sdb a = Value a (Tree a) (Tree a)

el :: Eq a => Sdb a -> a -> Bool
el (Value a b c) x
    | a == x = True
    | otherwise = elTree b x || elTree c x

elTree :: Eq a => Tree a -> a -> Bool
elTree (Node a b c) x
    | a == x = True
    | otherwise = elTree b x || elTree c x
elTree Empty _ = False

eq :: Eq a => Sdb a -> Sdb a -> Bool
eq (Value a1 b1 c1) (Value a2 b2 c2) = a1 == a2 && ((eqTree b1 b2 && eqTree c1 c2) || (eqTree b1 c2 && eqTree c1 b2))

eqTree :: Eq a => Tree a -> Tree a -> Bool
eqTree Empty Empty = True
eqTree (Node a1 b1 c1) (Node a2 b2 c2) = a1 == a2 && ((eqTree b1 b2 && eqTree c1 c2) || (eqTree b1 c2 && eqTree c1 b2))
eqTree _ _ = False

sdb2list :: Sdb a -> [a]
sdb2list (Value a b c) = a:treeBFS [b, c]

treeBFS :: [Tree a] -> [a]
treeBFS [] = []
treeBFS xs = [a | (Node a _ _) <- xs] ++ treeBFS ([b | (Node _ b _) <- xs] ++ [c | (Node _ _ c) <- xs])