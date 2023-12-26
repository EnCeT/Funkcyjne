hasSum:: Int->[[Int]]->Bool 
hasSum suma list = length (filter (\x-> sum x == suma) list) /= 0 

diffsums:: [[Int]]->[[Int]]
diffsums (l:list) = foldl(\acc x -> if hasSum (sum x) acc then acc else acc++[x]) [l] list