data Calkowite = Zero | Poprzednik Calkowite | Nastepnik Calkowite deriving Show

toInt :: Calkowite -> Int
toInt Zero = 0
toInt (Nastepnik a) = 1 + toInt a
toInt (Poprzednik a) = (-1) + toInt a
 
intToCalkowite :: Int -> Calkowite
intToCalkowite a
 | a == 0 = Zero
 | a > 0 = Nastepnik (intToCalkowite (a - 1))
 | a < 0 = Poprzednik (intToCalkowite (a + 1))