data Mb a = Mb [a] [a]

dnp :: Mb a -> a -> Mb a
dnp (Mb l r) el = Mb (el : l) r

dnk :: Mb a -> a -> Mb a
dnk (Mb l r) el = Mb  l (el : r)

mb2list :: Mb a -> [a]
mb2list (Mb l r) = l++(reverse r)

ull :: Mb a -> Mb a
ull (Mb l r) = Mb [] r

upl :: Mb a -> Mb a
upl (Mb l r) = Mb l []
