data Rd a = Solo a | Node1 a [Rd a]
-- elforAll :: Eq a => [Rd a] -> a -> Bool
-- elforAll [] elem = False
-- elforAll (r:rod) elem = el r elem || elforAll rod elem

-- substForAll:: Eq a => a -> a -> [Rd a] -> [Rd a]
-- substForAll old new [] = []
-- substForAll old new (r:rod) =  substForAll old new rod ++ [(subst old new r)]

rd2listAll:: [Rd a] -> [a]
rd2listAll  [] = []
rd2listAll (r:rod) = rd2list r ++ rd2listAll rod

el :: Eq a => Rd a -> a -> Bool
el (Solo a) elm = if a ==elm then True else False
el (Node1 a rod) elm = a == elm || any (==True) (map(\x -> el x a) rod)

subst :: Eq a => a -> a -> Rd a -> Rd a
subst old new (Solo a)  = if a ==old then Solo new else Solo a
subst old new (Node1 a rod) = if a == old then (Node1 new (mappedRod rod)) else (Node1 a (mappedRod rod))
    where mappedRod rdsToMap = map(\x->subst old new x) rdsToMap

rd2list :: Rd a -> [a]
rd2list (Solo a)  = [a]
rd2list (Node1 a rod) = [a]++ rd2listAll rod