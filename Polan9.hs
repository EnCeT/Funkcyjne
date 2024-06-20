import System.Environment
import System.IO
-- Napisać program, który prosi o podanie liczby naturalnej, a
-- następnie wypisuje na standardowe wyjście silnie podanej liczby.
-- Zad.1
silnia :: Integer -> Integer
silnia 0 = 1
silnia n = n * silnia (n-1)

-- main :: IO ()
-- main = do
--     putStrLn "Podaj liczbe naturalna: "
--     n <- readLn
--     putStrLn $ show n ++ "! = " ++ show (silnia n)
--     listFormat $ fibb n

-- Napisać funkcję o sygnaturze
-- displayList :: [Integer] -> IO ()
-- wypisującą podaną listę na standardowe wyjście. Wykorzystać ją
-- do napisania programu, który prosi nas o liczbę n, a następnie
-- wypisuje n pierwszych wyrazów ciągu Fibonacciego
-- Zad.2
fibb :: Integer -> [Integer]
fibb 0 = [0]
fibb 1 = [1,0]
fibb n = (head (fibb (n-1)) + head (tail (fibb (n-1)))) : fibb (n-1)

listFormat :: [Integer] -> IO ()
listFormat [] = putStrLn ""
listFormat x = putStr "[" >> displayList x >> putStr "]"

displayList :: [Integer] -> IO ()
displayList [] = putStrLn ""
displayList [x] = putStr $ show x
displayList x = do
    if last x == 0 then
        putStr ""
    else
        putStr $ show (last x) ++ ", "
    displayList $ init x

-- main :: IO ()
-- main = do
--     putStrLn "Podaj liczbe naturalna: "
--     n <- readLn
--     listFormat $ fibb n

-- Napisać program, który wypisuje na standardowe wyjście liczbę
-- znaków w pliku tekstowym podanym jako pierwszy argument
-- wywołania.
-- Zad.3
counter :: String -> Int
counter = length

-- main :: IO ()
-- main = do
--     (file:_) <-getArgs
--     content <- openFile file ReadMode
--     text <- hGetContents content
--     putStrLn $ show $ counter text
--     hClose content

-- Napisać program primes, który dla wywołania primes n outfile
-- zapisuje do pliku outfile listę n kolejnych liczb pierwszych (od 2).
-- Zad.4
primes :: Int -> [Int]
primes n = take n [x | x <- [2..], isPrime x]

isPrime :: Int -> Bool
isPrime n = null [x | x <- [2..n-1], n `mod` x == 0]

write :: Handle -> Int -> IO ()
write handle x = do
    hPutStr handle (show x)
    hPutStr handle "\n"

main :: IO ()
main = do
    (n:file:_) <- getArgs
    let list = primes (read n :: Int)
    handle <- openFile file WriteMode
    mapM_  (write handle) list
    hClose handle
