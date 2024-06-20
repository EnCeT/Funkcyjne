-- Maybe monad

-- data Maybe a = Nothing | Just a 

-- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
-- Nothing >>= _ = Nothing
-- Just x >>= f = f x

dzielsto :: Float -> Maybe Float
dzielsto 0.0 = Nothing
dzielsto x = Just (100.0 / x)
-- just 5.0 >>= dzielsto
-- just 0.0 >>= dzielsto


pierw :: Float -> Maybe Float
pierw x = if x < 0.0 then Nothing else Just (sqrt x)
-- just 5.0 >>= dzielsto >>= pierw

odejmijsto :: Float -> Maybe Float
odejmijsto x = Just (x - 100.0)
-- just 5.0 >>= dzielsto >>= odejmijsto
-- just 5.0 >>= dzielsto >>= odejmijsto >>= pierw
-- return 5.0 >>= dzielsto >>= odejmijsto >>= pierw

-- Monada listowa
razydwalubnie :: Int -> [Int]
razydwalubnie x = [x, 2*x]
-- return 5 >>= razydwalubnie

-- QuickSort
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort [y | y <- xs, y <= x] ++ [x] ++ quickSort [y | y <- xs, y > x]

--fajne sito erastonesa 
sito :: [Integer] -> [Integer]
sito (x:xs) = x : sito [y | y <- xs, y `mod` x > 0]
sito [] = [] -- dzięki temu działa to dobrze na skończonej liście

-- takeWhile (<100) (sito [2..])
-- takeWhile (<100) (sito [2..1000])

dzielniki :: Integer -> [Integer]
dzielniki n = [x | x <- [1..n], n `mod` x == 0]
-- return