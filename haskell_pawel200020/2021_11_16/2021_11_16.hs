remove :: Eq a => a -> [a] -> [a]
remove e xs = filter (\x -> e /= x) xs

qsort :: Ord a=>[a]->[a]
qsort [] = []
qsort (x:xs) = left ++ [x] ++ right
    where 
        left = qsort  [a | a <- xs, a <= x]
        right = qsort [a | a <- xs, a > x]

