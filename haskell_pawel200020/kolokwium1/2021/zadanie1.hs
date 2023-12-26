type DirectedGraph = ([Int], Int -> Int -> Bool)

listofNodes ::DirectedGraph -> Int -> [Int]
listofNodes (array, f) value = filter (\x->(f value x)) array

listofManyNodes ::DirectedGraph -> Int ->Int-> [Int]
listofManyNodes g value 0 = []
listofManyNodes g value 1 = listofNodes g value
listofManyNodes (array, f) value depth = (listofNodes (array, f) value)++ concatMap(\x->x)(map(\x->(listofManyNodes (array, f) x (depth -1)))(listofNodes (array, f) value))

atDistance :: DirectedGraph -> Int -> Int -> [Int]
atDistance (array, f) d v = listofManyNodes (array, f) d v
