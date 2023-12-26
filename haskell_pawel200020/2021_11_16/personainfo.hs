data PersonalInfo = PI String String Integer

-- typ przechowyjacy informacje nt. osoby: imie nazwisko oraz rok urodzenia.

instance Eq PersonalInfo where
  (==) = eqFunction
   where eqFunction (PI x y z) (PI a b c) = x == a && y == b && z == c


instance Show PersonalInfo where
  show = showFunction 
      where showFunction (PI imie nazwisko rok) = "Name: "++ imie ++"\n" ++ "Nazwisko: "++ nazwisko ++"\n" ++ "rok: "++ show rok 
