maksimum :: [Int]-> Int 
maksimum [] = 0
maksimum (x:xs) = if x > maximum xs then x else maksimum xs

f :: [Int] -> (Int, Int)
f [x] = if x > 0 then (x, 0) else (0, x)
f (x : xs) = if x > 0 then (fst (f xs) + x, snd(f xs)) else (fst (f xs), snd(f xs) + x) 

commonPrefix :: String -> String -> String
commonPrefix a [] = a
commonPrefix [] b = b
commonPrefix (a:as) (b:bs) =  if (a == b) then [a] ++ commonPrefix as bs else []

int2Bin :: Int -> [Int]
int2Bin 0 = [0]
int2Bin 1 = [1]
int2Bin x = int2Bin (x `div` 2) ++ [x `mod` 2]

bin2int :: [Int] -> Int
bin2int [0] = 0
bin2int [1] = 1
bin2int (x:xs) = x*2^(length xs) + bin2int xs

merge :: [Int] -> [Int] -> [Int]
merge [] a = a
merge b [] = b
merge (a:as) (b:bs) = if a <= b then [a] ++ merge as (b:bs) else [b] ++ merge (a:as) bs

mergeSort :: [Int] -> [Int]
mergeSort [x] = [x]
mergeSort x = merge (mergeSort a) (mergeSort b)
    where
        a = take (div (length x) 2) x
        b = drop (div (length x) 2) x

--homework


func:: [[Int]] -> [[Int]]
func [] = []
func (x:xs) = (x++[0]) : (x++[1]) : func xs

func2 :: Int -> [[Int]] -> [[Int]]
func2 1 y = y
func2 n y = func2 (n-1) (func y)   

allBin :: Int -> [[Int]]
allBin 0 = []
allBin n = func2 n [[0],[1]]


divisorToArray :: Int -> [Int]
divisorToArray n = [ x | x <- [1..n], n `mod` x == 0]

divisorToSum :: [Int] -> Int 
divisorToSum [x] = x
divisorToSum (x:xs) = x + divisorToSum xs

divisorSum :: Int -> Int
divisorSum n = divisorToSum (divisorToArray n)