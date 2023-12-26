-- Zadanie 1. Rozwa˙zmy ci ˛agi znaków zło˙zone z liter a, b i c, w których liczba wyst ˛apien´
-- litery a jest dwa razy mniejsza ni˙z suma liczb wyst ˛apien liter ´ b i c. Napisa´c funkcj˛e, która
-- dla argumentu naturalnego n zwróci list˛e wszystkich takich ci ˛agów znaków o długo´sci 3n.
-- Przykładowo dla argumentu 1 powinna by´c to lista zło˙zona z ci ˛agów "abb", "abc", "acb",
-- "acc", "bab", "bac", "cab", "cac", "bba", "bca", "cba", "cca". Kolejno´s´c ci ˛agów
-- w li´scie jest dowolna.
-- Zadanie.1
podzbiory :: Int-> [String]
podzbiory 0  = [""]
podzbiory n = map ('a':) (podzbiory (n-1)) ++ map ('b':) (podzbiory (n-1)) ++ map ('c':) (podzbiory (n-1))

numa :: String -> Int
numa x = length (filter (=='a') x)

pc :: Int -> [String]
pc n = filter (\x -> n == numa x) l
    where 
        l = podzbiory (3*n)
