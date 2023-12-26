repl1 :: Eq a => [a] -> (a, a) -> [a]
repl1 list (a,b) = foldl (\acc x -> if x == a then  acc++[b] else acc++[x]) [] list

repl :: Eq a => [a] -> [(a, a)] -> [a]
repl list [(x,y)] = repl1 list (x,y)
repl list ((x,y):xs) = repl (repl1 list (x,y)) xs