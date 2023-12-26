stir:: Int -> Int -> Int 
stir 0 0 = 1
stir 0 k = 0
stir k 0 = 0
stir n k = stir (n-1) (k-1)+k*(stir (n-1) k)

fun:: Int -> Int
fun x = x


sumSeq :: Int -> (Int -> Int) -> Int 
sumSeq 0 f = f 0
sumSeq n f = f n + sumSeq (n-1) f

bell :: Int ->  Int-> (Int -> Int-> Int)  -> Int
bell 0 0 f = f 0 0
bell n k f = f n k + bell (n-1) k f

maxlist :: [Int]-> Int 
maxlist [] =0
maxlist (x:xs) = if maxlist xs < x then x else maxlist xs

sumPosNeg :: [Int] -> (Int,Int)
sumPosNeg [x] = if x>0 then (x,0) else (0,x)
sumPosNeg (x:xs) = if x>0 then ( fst (sumPosNeg xs)+ x ,snd (sumPosNeg xs)) else (fst (sumPosNeg xs),snd (sumPosNeg xs)+x) 

commonPrefix :: String -> String ->String 
commonPrefix a [] = a
commonPrefix [] b = b
commonPrefix (a:as) (b:bs) = if(a==b) then [a] ++ commonPrefix as bs else commonPrefix as bs

intToBin :: Int -> [Int]
intToBin 0 = [0]
intToBin 1 = [1]
intToBin n = intToBin (n`div`2) ++ [n `mod` 2]

binToInt :: [Int] -> Int
binToInt [0] = 0
binToInt [1] = 1
binToInt (x:xs) = x * 2^length xs + binToInt xs

merge:: [Int]-> [Int]-> [Int]
merge [] [x] = [x]
merge [x] [] = [x]
merge (a:as) (b:bs) = if a < b then  [a]++ merge as ([b]++bs) else [b] ++ merge ([a]++as) bs

mergeSort:: [Int]->[Int]
mergeSort [x] = [x]
mergeSort x = merge (mergeSort a)  (mergeSort b)
    where
        a = take (div (length x) 2) x
        b = drop (div (length x) 2) x

func3 :: [[Int]]->[[Int]]
func3 [] = []
func3 (x:xs)= (x++[0]) : (x++[1] ): func3 xs

func2 :: Int -> [[Int]] -> [[Int]]
func2 1 n = n
func2 n x = func2 (n-1) (func3 x)

allBin :: Int -> [[Int]]
allBin 0 = []
allBin n = func2 n [[0],[1]]

divisorToArray:: Int -> Int -> [Int]
divisorToArray n 1 = [1]
divisorToArray n k= if n `mod` k == 0 then k : divisorToArray n (k-1) else divisorToArray n (k-1)

divisorSum1:: [Int] -> Int
divisorSum1 []=0
divisorSum1(x:xs) = x+ divisorSum1 xs


divisorSum :: Int -> Int
divisorSum n = divisorSum1 (divisorToArray n n)