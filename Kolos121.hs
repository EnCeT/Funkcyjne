--Zad.1

--Zad.2
wil:: Int -> [[Int]] -> Int
wil n [[]] = 0
wil n [x] = if n `elem` x then 1 else 0
wil n (x:xs) = if n`elem` x then 1 + wil n xs else wil n xs

wIluListach:: Int -> [[Int]] -> [Int]
wIluListach 1 x = [wil 1 x]
wIluListach n x = wIluListach (n - 1) x ++ [wil n x]   

--Zad.3
data Wr a = 

dg :: Wr a -> [a] -> Wr a
ug :: Wr a -> Wr a
de :: Wr a -> a -> Wr a
ue :: Wr a -> Wr a
lg :: Wr a -> Integer
wr2l :: Wr a -> [a]