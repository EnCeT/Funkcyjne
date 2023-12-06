--Zad.1

--Zad.2

--Zad.3
c :: Integer -> [Integer] -> Bool
c 1 x = 1 `elem` x 
c n x = if n `elem` x then c (n-1) x else  False

cp::[Integer] -> [Integer]
cp [x] = if x == 1  then [x] else []
cp x = if c n x then  cp (take (m - 1) x) ++ [n] else cp (take (m - 1) x )
        where
             n = fromIntegral (length x) 
             m = length x