data Wr a = Wr [[a]] Bool Int

dg :: Wr a -> [a] -> Wr a
dg (Wr a canAdd size) array = (Wr (array:a) True (size+1))

ug :: Wr a -> Wr a
ug (Wr (a:as) canAdd size) = (Wr(as) False (size-1))

de :: Wr a -> a -> Wr a
de (Wr (a:as) False size) elem = Wr ((a):as ) False size
de (Wr (a:as) True size) elem = Wr ((elem:a):as) True size

ue :: Wr a -> Wr a
ue (Wr ((elem:a):as) False size)= Wr ((elem:a):as) False size
ue (Wr ((elem:a):as) True size)= Wr ((a):as) True  size

lg :: Wr a -> Integer
lg (Wr (a) _ size) = toInteger size

wr2l :: Wr a -> [a]
wr2l (Wr (a) _ _) = concatMap (\x -> x) a 