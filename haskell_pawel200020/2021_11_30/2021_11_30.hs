
bin2Int::[Int]->Int
bin2Int (x:xs)= foldl(\acc y -> acc*2+y) 0 xs

getSum::[Int]->Int 
getSum[]=0
getSum x = foldl(\acc a -> acc+a) 0 x

hasSum ::Int->[[Int]]->Bool
hasSum _ [] = False
hasSum a (x:xs)= if a==getSum x then True else  hasSum a xs

diffSums :: [[Int]]->[[Int]]
diffSums (x:xs) = foldl(\acc y->(if (hasSum (getSum y) acc) then acc else acc++[y])) [x] xs

ps :: [a] -> [[a]]
ps list = foldl prefixFun [] list ++ tail (foldr sufixFun [] list)
    where
        prefixFun :: [[a]] -> a -> [[a]]
        prefixFun [] a = [[a]]
        prefixFun list a = list ++ [last list ++ [a]]
 
        sufixFun :: a -> [[a]] -> [[a]]
        sufixFun a [] = [[a]]
        sufixFun a list = (a:head list) : list

