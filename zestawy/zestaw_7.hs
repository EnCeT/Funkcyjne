data Student = Student {
    imie :: String,
    nazwisko :: String,
    nrAlbumu :: Int
}
instance Show Student where show (Student a b c) = "Imie: " ++ show a ++ "\nNazwisko: " ++ show b ++ "\nnrAlbumu: " ++ show c


data Calkowite = Zero | Nast Calkowite | Popr Calkowite 
    deriving (Show,Eq)
calkToInteger :: Calkowite -> Integer
calkToInteger Zero = 0
calkToInteger (Nast x) = (calkToInteger x) + 1
calkToInteger (Popr x) = (calkToInteger x) - 1

integertoCalk :: Integer -> Calkowite
integertoCalk n
    | n > 0 = Nast (integertoCalk (n-1))
    | n < 0 = Popr (integertoCalk (n+1))
    | otherwise = Zero


data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq)
drzewo :: Tree Integer
drzewo = Node 2 (Node 2 (Node 6 Empty Empty) (Node 4 Empty Empty)) Empty

czyParzyste :: Tree Integer -> Bool
czyParzyste (Node a b c) = even a && czyParzyste b && czyParzyste c
czyParzyste Empty = True

preorder :: Tree Integer -> [Integer]
preorder (Node a b c) = a : preorder b ++ preorder c
preorder Empty = []

data Gauss = Gauss Integer Integer

instance Num Gauss where
    (Gauss a b) + (Gauss c d) = Gauss (a+c) (b+d)
    (Gauss a b) - (Gauss c d) = Gauss (a-c) (b-d)
    (Gauss a b) * (Gauss c d) = Gauss (a*c) (b*d)
    fromInteger x = Gauss x 0
    abs (Gauss a b) = Gauss a b
    signum (Gauss _ _) = 1

instance Show Gauss where show (Gauss a b) = show a ++ " + " ++ show b ++ "i"