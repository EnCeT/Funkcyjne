revl :: [a] -> [a]
revl = foldl (flip (:)) []

revr :: [a] -> [a]
revr = foldr (\x xs -> xs ++ [x]) []

mapr :: (a -> b) -> [a] -> [b]
mapr f = foldr (\x xs -> f x:xs) []

mapl :: (a -> b) -> [a] -> [b]
mapl f = foldl (\x xs -> x ++ [f xs]) []

doDziesietnego :: Int -> [Int] -> Int
doDziesietnego n l = fst (foldr (\x (a1, a2) -> (a1 + x*n^a2, a2 + 1)) (0, 0) l)

cwiartka :: (Int, Int) -> [Int]
cwiartka tuple
  | fst tuple > 0 && snd tuple > 0 = [1, 0, 0, 0]
  | fst tuple < 0 && snd tuple > 0 = [0, 1, 0, 0]
  | fst tuple < 0 && snd tuple < 0 = [0, 0, 1, 0]
  | fst tuple > 0 && snd tuple < 0 = [0, 0, 0, 1]
  | otherwise = [0, 0, 0, 0]

maksimum :: [Int] -> Int
maksimum lista = foldl (\acc x -> if lista !! x > lista !! acc then x else acc) 0 [1 .. length lista - 1]

ktoraCwiartka :: [(Int, Int)] -> Int
ktoraCwiartka lista = maksimum (foldl (\acc x -> zipWith (+) (cwiartka x) acc) [0, 0, 0, 0] lista) + 1

dlaKazdego :: (a -> Bool) -> [a] -> Bool
dlaKazdego f = foldr (\x y -> (f x) && y) True

istnieje :: (a -> Bool) -> [a] -> Bool
istnieje f = foldr (\x y -> (f x) || y) False