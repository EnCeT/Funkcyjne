-- Zadanie 1. Napisa´c interaktywny program udaj ˛acy wszechwiedz ˛ac ˛a sztuczn ˛a inteligencj˛e, odpowiadaj ˛ac ˛a na pytania tak/nie. Program powinien poprosi´c o pytanie, wczyta´c je ze standardowego wej´scia, a nast˛epnie odpowiedzie´c (na standardowe wyj´scie) twierdz ˛aco, je´sli w pytaniu znajdowało si˛e parzy´scie wiele samogłosek, oraz przecz ˛aco w przeciwnym przypadku.
-- Mo˙zna zało˙zy´c, ˙ze pytanie składa si˛e tylko z małych i du˙zych liter (bez polskich znaków), liczb,
-- przecinków i pytajników.
-- Zad.1
samogloski :: String -> Int
samogloski [] = 0
samogloski (x:xs) 
    | x == 'a' || x == 'e' || x == 'i' || x == 'o' || x == 'u' = 1 + samogloski xs
    | x == 'A' || x == 'E' || x == 'I' || x == 'O' || x == 'U' = 1 + samogloski xs
    | otherwise = samogloski xs

main :: IO ()
main = do
    putStrLn "Podaj pytanie"
    z <- getLine
    let a = samogloski z
    if (a `mod` 2 == 0) then putStrLn "Tak" else putStrLn "Nie"
    