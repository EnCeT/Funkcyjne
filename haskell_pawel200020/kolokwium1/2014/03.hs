h2:: Int -> [a]->[a]
h2 _ [] = []
h2 i (l:list) = if even i then l : h2 (i+1) list else h2 (i+1) list

h:: [a]-> [a]
h  = h2 0