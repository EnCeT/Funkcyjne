hasElement:: [Int]->Int -> Bool 
hasElement [] _ = False 
hasElement (x:ss) a = if x==a then True else hasElement ss a

howManyElement:: [[Int]]->Int -> Int 
howManyElement array element = foldl(\acc x-> if(hasElement x element) then acc+1 else acc)0 array

wIluListach:: Int -> [[Int]] -> [Int]
wIluListach n array = foldl(\acc x-> acc++[(howManyElement array x)]) [] [1..n]