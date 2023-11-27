--Zad 1 
wielkieLitery :: [String] -> [Char]
wielkieLitery y = if head y `elem` ['A'..'Z'] then [head y] else []
wielkieLitery (y : ys) =  if  head y `elem` ['A'..'Z'] then head y ++ wielkieLitery ys else wielkieLitery ys

