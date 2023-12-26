-- data Sdb a = Solo a | Node1 a  (Sdb a) | Node2 a  (Sdb a) (Sdb a)

-- el :: Eq a => Sdb a -> a -> Bool
-- el (Solo a) em = a == em
-- el (Node1 a  subnode) em = (a == em) || el subnode em
-- el (Node2 a  subnode1 subnode2) em = (a == em) || el subnode1 em || el subnode2 em


-- eq :: Eq a => Sdb a -> Sdb a -> Bool
-- eq (Solo a) (Solo b) = a == b
-- eq (Node1 a  subnode) (Node1 b subnode2)  = (a == b) && eq subnode subnode2
-- eq (Node2 a  subnode1 subnode2) (Node2 b  subnode3 subnode4) = (a==b) && ((eq subnode1 subnode3 && eq subnode2 subnode4) ||(eq subnode1 subnode4 && eq subnode2 subnode3))
-- eq _ _ = False

-- sdb2list :: Sdb a -> [a]
-- sdb2list (Solo a) = [a]
-- sdb2list  (Node1 a  subnode) = a : sdb2list subnode
-- sdb2list  (Node2 a  subnode1 subnode2) = a : sdb2list subnode1 ++ sdb2list subnode2
data Sdb a= Sdb0 a | Sdb1 a (Sdb a) |Sdb2 a (Sdb a) (Sdb a)
 
el :: Eq a => Sdb a -> a -> Bool
el (Sdb0 x) s = s==x
el (Sdb1 x t1) s =if s==x then True else (el t1 s)
el (Sdb2 x t1 t2) s =if s==x then True else (el t1 s) || (el t2 s)
 
eq :: Eq a => Sdb a -> Sdb a -> Bool
eq (Sdb0 w1) (Sdb0 w2) = w1==w2
eq (Sdb1 w1 t11) (Sdb1 w2 t12) = w1==w2 && eq t11 t12
eq (Sdb2 w1 t11 t21) (Sdb2 w2 t12 t22) = w1==w2 && ((eq t11 t12 && eq t21 t22) || (eq t11 t22 && eq t21 t21))
eq t1 t2 = False
 
sdb2list :: Sdb a -> [a]
sdb2list t  = concatMap (\x->x) (takeWhile (\x -> (length x)/=0)(map (\x-> help t x) [0..]))
    where
    help :: Sdb a->Int->[a]
    help (Sdb0 x) d = if d==0 then [x] else []
    help (Sdb1 x t1) d =if d==0 then [x] else help t1 (d-1)
    help (Sdb2 x t1 t2) d =if d==0 then [x] else (help t1 (d-1))++(help t2 (d-1))
    depth :: Sdb a ->Int
    depth (Sdb0 x) = 0
    depth (Sdb1 x t1) =1+(depth t1)
    depth (Sdb2 x t1 t2) =if (depth t1)>(depth t2) then 1+(depth t1) else 1+(depth t2)