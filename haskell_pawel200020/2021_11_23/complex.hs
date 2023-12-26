data MyComplexVal = MC  Integer Integer 

instance Show MyComplexVal where
  show = showFunction
      where showFunction (MC real im) = show real ++ " + "++show im ++"*i"

instance Num MyComplexVal where
  (MC a b) + (MC c d ) = MC (a+b) (c+d)
  (MC a b) * (MC c d ) = MC (a*b+ b*d*(-1)) (a*d+b*c)
  negate (MC a b) = MC (-a) (-b)
  fromInteger a = MC a 0
  abs (MC a b) =  MC (abs a) (abs b)
  signum (MC a b) | a==0 && b == 0 = MC 0 0
                  | a>0 && b>0 = MC 1 1
                  | a<0 && b<0 = MC (-1) (-1)
                  | a<0 && b>0 = MC (-1) 1
                  | a>0 && b<0 = MC 1 (-1)
