transformList:: [a]->[a]
transformList (l:list) = list++[l]

cykl:: [a]->[[a]]
cykl list = foldl (\acc x-> (acc++[(transformList (last acc))])) [list]  [1..(length list -1)]