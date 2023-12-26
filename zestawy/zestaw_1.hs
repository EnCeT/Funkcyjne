stirling :: Int -> Int -> Int
stirling 0 0 = 1
stirling n k | n < 1 = -1
             | n == 0 && k == 0 = 1
             | n == k = 1
             | k == 0 = 0
             | otherwise = (n-1)*(stirling (n-1) k) + (stirling (n-1) (k-1))

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 x = head x * fun1 (tail x)

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x < y = x:(merge xs (y:ys))
          | otherwise = y:(merge ys (x:xs))

mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort (take half xs)) (mergeSort (drop half xs))
    where half = length xs `div` 2