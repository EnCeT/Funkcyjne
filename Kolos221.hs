-- Zadanie 2. Napisa´c funkcj˛e usun :: String -> String -> String, która ze stringu podanego jako pierwszy argument usuwa wszystkie znaki, które wyst˛epuj ˛a w stringu podanym jako drugi argument. Definicja funkcji musi rozpoczyna´c si˛e od wywołania foldl
-- lub foldr. Przykład:
-- usun "Ala ma kota" "kra" = "Al m ot"

usun :: String -> String -> String
usun str charsToDelete = foldl (\acc c -> if c `elem` charsToDelete then acc else acc ++ [c]) "" str

main :: IO ()
main = do
    let result = usun "Ala ma kota" "kra"
    putStrLn result