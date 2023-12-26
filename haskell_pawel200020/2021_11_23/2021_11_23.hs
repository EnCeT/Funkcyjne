reverse2:: [a]->[a]
reverse2 [] = []
reverse2 (x:xs) = reverse2 xs ++ [x]

reverseFold2 :: [a] -> [a]
reverseFold2 (a:as)= foldl (\acc x -> x: acc) [a] as

horner2:: Num a => [a]->a->a
horner2 (a:as) x0 = foldl(\acc x->acc*x0+x) a as

horner :: [Int] -> Int -> Int
horner f x0 = foldl (\acc x -> acc*x0 + x) 0 f



quantity::  Eq a =>[a]->a->Int
quantity a w = foldl(\acc n -> if(n == w) then acc+1 else acc) 0 a

added:: Eq a =>[(a,Int)]->a->Bool
added [] _ =False
added ((b, s):xs) a =if(a==b) then True else added xs a

freqHelp:: Eq a => [a]-> [(a,Int)]
freqHelp [] = []
freqHelp (x:xs) = (x, quantity (x:xs) x) : freqHelp (removeEl xs x)
    where removeEl list a = filter (/= a) list

insertElement :: Ord a => a -> [a] -> [a]
insertElement a [x] = if a<x  then a:[x] else x:[a]
insertElement a (x:xs)= foldl(\acc b ->
    if a> last acc && a<b
        then acc++[a]++[b]
        else
            if length acc==1 && (a<last acc && a<b)
                then [a]++acc++[b]
                else
                    if length acc==length xs && (a>last acc && a>b)
                        then acc++[b]++[a]
                        else
                            acc++[b]) [x] xs

insertionSort:: Ord a => [a] -> [a]
insertionSort (x:xs)=foldl(\acc y -> insertElement y acc) [x] xs













sumList :: (Num a)=> [a] -> a
sumList [] = 0
sumList (x:xs) = foldl (+) x xs

maxElement :: [Int] -> Int
maxElement [] = 0
maxElement (x:xs) = foldl (max) x xs

