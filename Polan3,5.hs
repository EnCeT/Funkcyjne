--Zad 7.
--map :: (Ord a,Num a ) => (a -> a) -> [a] -> [a]
--map f xs = zipWith f xs undefined 

zip :: [a] -> [a] -> [(a,a)]
zip = zipWith (,)

--Zad 8.
sito :: [Integer] -> Integer -> [Integer]
sito list n = if n*n <= last list then sito (filter (\x -> x `mod`n > 0 || x == n) list) (n + 1) else list

eratostenes:: Integer -> [Integer]
eratostenes n = sito list 2
            where 
                list = [2..n]