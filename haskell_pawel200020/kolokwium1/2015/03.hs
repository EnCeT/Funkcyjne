data Klos a = Klos [a] [a]

wnpk :: Klos a -> a -> Klos a
wnpk (Klos p k) elem =  (Klos (elem:p) k)

wnkk :: Klos a -> a -> Klos a
wnkk (Klos p k) elem =  (Klos p (elem:k))

k2list :: Klos a -> [a]
k2list (Klos p k) = p++k