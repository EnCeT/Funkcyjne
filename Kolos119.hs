--Zad.1
l:: Int -> Int -> Int
l n x | (n `div` x) >= 10 = l n (x*10) 
      | otherwise  = x

suma:: Int -> Int
suma n = if n >= 10 then y + suma (n `mod` x) else n
    where 
        x = l n 10
        y = n `div` x 

s:: Int -> Int
s n = if x > 15 then s x else x
    where 
        x = suma n

sevens:: Int -> [Int]
sevens n = take n (filter (\x -> (s x) == 7 ) [1..])

--Zad.2
podzbiory :: Int-> [String]
podzbiory 0  = [""]
podzbiory n = map ('a':) (podzbiory (n-1)) ++ map ('b':) (podzbiory (n-1))

parzyste :: [String] -> [String]
parzyste [] = []
parzyste (x:xs) | even (length x) = [x] ++ parzyste xs
                | otherwise = parzyste xs  

balance :: String -> Bool
balance x = (length (filter ('a' ==) x)) == (length (filter ('b' ==) x))


bp:: Int -> [String]
bp n = map (\x -> x ++ reverse x) (filter (\x -> balance x) p)
    where
        p = if even n then podzbiory (n `div` 2) else []
