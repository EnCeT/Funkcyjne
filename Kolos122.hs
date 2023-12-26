--Zadanie.3
data Bsk a = Empty | Elem a Int [a]

de:: (Eq a) => Bsk a -> a -> Bsk a
de Empty x = Elem x 1 [x]
de (Elem first cou list) x = Elem first (if x == first then cou + 1 else cou) (x:list)

oe :: Bsk a -> a
oe (Elem _ _ (x:_)) = x

ue :: (Eq a) => Bsk a -> Bsk a
ue (Elem _ _ [_]) = Empty
ue (Elem p n (x:xs)) = Elem p (if x == p then n - 1 else n) xs

le:: Bsk a -> Int
le (Elem _ l _) = l