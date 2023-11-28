import Distribution.Simple.Program.HcPkg (list)

--Zad 1 
--wielkieLitery :: [String] -> [Char]
--wielkieLitery y = if head y `elem` ['A'..'Z'] then [head y] else []
--wielkieLitery (y : ys) =  if  head y `elem` ['A'..'Z'] then head y ++ wielkieLitery ys else wielkieLitery ys

--Zad 2
isItPalindrom :: String -> Int
isItPalindrom s | s == reverse s = length s
                | otherwise = 0

dlugoscPalindromow :: [String] -> Int
dlugoscPalindromow [x] = isItPalindrom x
dlugoscPalindromow (x:list) = isItPalindrom x + dlugoscPalindromow list

dlugoscPalindromow2 :: [String] -> Int
dlugoscPalindromow2 list = filter (\x -> length x == length (reverse x)) list
