import System.Environment
import System.IO
-- Zadanie 1. Napisa´c interaktywny program do gry w „papier, kamien, no ´ ˙zyce”. Komputer ma
-- wykonywa´c seri˛e kolejnych ruchów zgodnie z kolejnymi znakami stringu, podanego jako parametr podczas uruchomienia programu. Mo˙zna zało˙zy´c, ˙ze string zawiera dodatni ˛a liczb˛e
-- znaków ze zbioru {P, K, N}. Uruchomienie programu ma wykona´c tyle rund gry, ile znaków
-- jest w stringu; w ka˙zdej rundzie program prosi o wpisanie naszego ruchu (tj. znaku P, K lub N),
-- po czym wypisuje swoj ˛a odpowied´z (na podstawie stringu) i bie˙z ˛acy stan rozgrywki (liczb˛e
-- całkowit ˛a, oznaczaj ˛ac ˛a bilans rozgrywki na rzecz komputera), a nast˛epnie przechodzi do kolejnej rundy. Po wyczerpaniu ruchów program konczy działanie.

--Zad.1
gra :: Char -> Char -> Int
gra x y 
    | x == 'P' && y == 'K' = 1
    | x == 'P' && y == 'N' = -1
    | x == 'K' && y == 'P' = -1
    | x == 'K' && y == 'N' = 1
    | x == 'N' && y == 'P' = 1
    | x == 'N' && y == 'K' = -1
    | otherwise = 0

write :: String -> Char -> Int -> IO ()
write [] _ r = putStrLn ("Koniec gry wynik: " ++ show r)
write (x:xs) y r = do
    putStrLn "Podaj ruch"
    z <- getLine
    let a = head z
    let b = gra a x
    putStrLn ("Stan gry: " ++ (show (r + b)))
    write xs y (r+b)

main :: IO ()
main = do
    args <- getArgs
    -- let l = length args
    let (x:xs) = args 
    write x 'X' 0
    putStrLn "-------"