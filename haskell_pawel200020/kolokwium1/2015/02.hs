val2 :: Integer -> Integer ->Integer -> Integer
val2 a b pow
  |  b>a = -1
  | ((a`mod`b^pow) == 0) && ((a`mod`b^(pow+1)) /= 0) = pow
  | ((a`mod`b^pow) /= 0) && ((a`mod`b^(pow+1)) /= 0) = 0
  | otherwise = val2 a b (pow+1)

val :: Integer -> Integer -> Integer
val a b = val2 a b 1

gbis :: Integer -> Integer ->Integer-> [Integer]
gbis k v n = if(okVal n k v) then [n]++gbis k v (n+1) else gbis k v (n+1)
    where okVal n k v = (val n k == v)

g :: Integer -> Integer -> [Integer]
g k v = gbis k v 1