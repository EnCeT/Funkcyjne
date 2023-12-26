fun1 :: (Eq a) => a -> (a -> a) -> Bool
fun1 a g = a == g a

fun2 :: (Ord a, Num a, Show a) => a -> a -> String
fun2 a b 
    | (a + b) < (a * b) = "Product of " ++ show a ++ " " ++ show b ++ " bigger than sum"
    | otherwise = "Product of " ++ show a ++ " " ++ show b ++ " smaller or equal than sum"

list1 :: [Integer]
list1 = [x | x <- [1..], mod x 6 == 1, mod x 7 == 4, mod x 8 == 3]

fun3 :: Int -> [Int]
fun3 x = [y | y <-[1..(x `div` 2)], mod x y == 0]

fun4 :: [(Int, Int)] -> Int
fun4 list = snd (maximum [first, second, third, fourth]) 
    where 
        first = (length [(x, y) | (x, y) <- list, x>=0, y>=0], 1)
        second = (length [(x, y) | (x, y) <- list, x<=0, y>=0], 2)
        third = (length [(x, y) | (x, y) <- list, x<=0, y<=0], 3)
        fourth = (length [(x, y) | (x, y) <- list, x>=0, y<=0], 4)

sublist :: [Integer] -> [[Integer]]
sublist [] = [[]]
sublist (x:xs) = [x:l | l <- sublist xs] ++ sublist xs

fun5 :: Integer -> [Integer] -> [[Integer]]
fun5 k list = [y | y <- sublist list, fromIntegral (length y) == k]

mymap :: (Int -> Int) -> [Int] -> [Int]
mymap f xs = [f x | x <- xs]