func:: [[Int]]->[[Int]]
func []= []
func list = map(1:) list ++ map(0:) list

func2:: Int ->[[Int]]-> [[Int]]
func2 1 array = array 
func2 n array = func2 (n - 1) (func array)

allBin :: Int -> [[Int]]
allBin 0 = []
allBin n = func2 n [[0], [1]]

notEvenOne:: [Int]-> Bool
notEvenOne list =  countOne `mod` 2 == 1
    where countOne = foldl(\acc x -> if x ==1 then (1+acc) else acc) 0 list

oddbins:: Int ->[[Int]]
oddbins n = filter notEvenOne (allBin n)