
--Zad 1 
maleLitery :: [String] -> [String]
maleLitery list = map (filter (\x -> elem x  ['a'..'z'])) list

--Zad 2
isItPalindrom :: String -> Int
isItPalindrom s | s == reverse s = length s
                | otherwise = 0

dlugoscPalindromow :: [String] -> Int
dlugoscPalindromow [x] = isItPalindrom x
dlugoscPalindromow (x:list) = isItPalindrom x + dlugoscPalindromow list

--Zad 3
f:: (Integer,Integer) -> (Integer,Integer)
f (x,y) = (y,x+y)


fib::(Integer,Integer) -> [(Integer,Integer)]
fib (x,y) = iterate f (x,y)

--Zad 4
dlugosc:: [a] -> Int
dlugosc list = sum (map (\x -> 1) list)

--Zad 5
slowaDlugosc:: Char -> Char -> Integer -> [String]
slowaDlugosc x y 0 = [""]
slowaDlugosc d b n = map (d:) (slowaDlugosc d b (n - 1)) ++ map (b:) (slowaDlugosc d b (n - 1))

--Zad 6.
low :: (Ord a) => [a] -> a -> [a]
low [x] q = [x]
low (x:xs) q = if x <= q then [x] ++ low xs q  else low xs q

high ::(Ord a) => [a] -> a -> [a]
high [x] q = if x > q then [x] else [] 
high (x:xs) q = if x > q then [x] ++ high xs q else high xs q

quickSort :: (Ord a) => [a] -> [a]
quickSort [x] = [x]
quickSort list = if length b > 0 then quickSort a ++ quickSort b else quickSort (init list) ++ quickSort [last list]
            where 
                (a,b) = (low list (last list), high list (last list))
                
