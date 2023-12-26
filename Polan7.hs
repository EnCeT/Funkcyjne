import Language.Haskell.TH (Guard)
--Zad.1
data Student = Student{imie :: String,
                       nazwisko :: String,
                       nrAlbumu :: Int
                       }
instance Show Student 
    where
               show student = ("Imie: " ++ imie student ++ "\nNazwisko: " ++ nazwisko student ++ "\nNr albumu: " ++ show (nrAlbumu student) ++ "\n")

s = Student {imie = "Karol", nazwisko = "Wojtyla", nrAlbumu = 2137}

--Zad.2
data Calkowite = Zero | Nastepnik Calkowite | Poprzednik Calkowite
    deriving (Show, Eq)

-- toInteger :: Calkowite -> Integer
-- toInteger Zero = 0
-- toInteger (Poprzednik x) = (toInteger x) - 1
-- toInteger (Nastepnik x) = (toInteger x) + 1

toCalkowite :: Integer -> Calkowite
toCalkowite 0 = Zero
toCalkowite x | x > 0 = Nastepnik (toCalkowite (x - 1))
              | x < 0 = Poprzednik (toCalkowite (x + 1))

--Zad.3
data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Eq, Show)

treelist :: Tree a -> [a]
treelist Empty = []
treelist (Node a x y) =  treelist x ++ [a] ++ treelist y


eve ::  (Integral a) => Tree a -> Bool
eve Empty = False
eve t = foldl (&&) True (map (even) (treelist t))

--Zad.4
inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node a x y) = inorder x ++ [a] ++ inorder y

preorder :: Tree a -> [a]
preorder Empty = []
preorder (Node a x y) = [a] ++ inorder x ++ inorder y

postorder :: Tree a -> [a]
postorder Empty = []
postorder (Node a x y) = inorder x ++ inorder y ++ [a]

--Zad.5
data Gaussa = Gaussa {rzeczywista :: Integer, urojona :: Integer}

instance Num Gaussa
    where 
        abs = id 
        signum _ =  Gaussa 1 0
        (+) (Gaussa rzeczywista1 urojona1) (Gaussa rzeczywista2 urojona2) = Gaussa (rzeczywista1+rzeczywista2) (urojona1+urojona2)
        (*) (Gaussa rzeczywista1 urojona1) (Gaussa rzeczywista2 urojona2) = Gaussa (rzeczywista1*rzeczywista2) (urojona1+urojona2)

instance Show Gaussa
    where
        show (Gaussa rzeczywista urojona) = show rzeczywista ++ "+" ++ show urojona ++ "i"

--Zad.6
data Matrix = Matrix ((Integer, Integer), (Integer, Integer))

instance Num Matrix
    where 
        (+) (Matrix ((a1,b1),(c1,d1))) (Matrix((a2,b2),(c2,d2))) = Matrix ((a1+a2,b1+b2),(c1+c2, d1+d2))
        (*) (Matrix ((a1,b1),(c1,d1))) (Matrix((a2,b2),(c2,d2))) = Matrix ((a1*a2 + c1*b1, a1*b2 + b1*d2),(c1*a2 + c2*d1, c1*b1 + d1*d2 ))
        abs = id
        signum _ = Matrix ((1,1),(1,1))

instance Show Martix
    where 
        show (Matrix ((a,b)(c,d))) = "[" ++ show a ++ " " ++ show b "]\n" ++ "[" ++ show c ++ " " ++ show d "]\n"
