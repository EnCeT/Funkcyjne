wordChanger:: String->String
wordChanger [] = []
wordChanger [x] = [x]
wordChanger (e1:e2:es) | e1 == 'a' && e2 == 'b' = "a"++wordChanger es
                       | e1 == 'b' && e2 == 'a' = "b"++wordChanger es
                       | e1 == 'b' && e2 == 'b' = "a"++wordChanger es
                       | e1 == 'a' && e2 == 'a' = "aaa"++wordChanger es

hasAlla :: String->Bool
hasAlla = foldr (\ x -> (&&) (x == 'a')) True 

dlugosc:: String -> Int 
dlugosc word = length (takeWhile (\x->(not(hasAlla x))&&(length x)>=2) (iterate wordChanger  word))
