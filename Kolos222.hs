-- Zadanie 1. Napisa´c program, który rysuje (w sensie ASCII art) choink˛e o zadanej wielko´sci
-- w nast˛epuj ˛acy sposób: pyta o wielko´s´c choinki (liczba naturalna dodatnia), a nast˛epnie wypisuje na standardowe wyj´scie choink˛e ˙z ˛adanej wielko´sci zło˙zon ˛a ze spacji oraz znaków /, \, ^
-- i |, składaj ˛ac ˛a si˛e z cz˛e´sci górnej, dolnej i pnia, przy czym:
-- • Cz˛e´s´c górna powinna w pierwszej linijce zawiera´c odpowiedni ˛a liczb˛e spacji, po której
-- nast˛epuj ˛a znaki /\.
-- • Ka˙zda kolejna linijka cz˛e´sci górnej powinna składa´c si˛e ze spacji i znaków / oraz \ w taki
-- sposób, aby znak / pojawił si˛e o jeden znak wcze´sniej ni˙z w poprzedniej linijce, a znak \
-- — o jeden znak pó´zniej.
-- • Ostatnia linijka cz˛esci górnej ma by´c tworzona analogicznie do poprzednich, ale ma nie
-- zaczyna´c si˛e od spacji (czyli choinka musi zosta´c wypisana „maksymalnie po lewej”).
-- • Cz˛e´s´c dolna powinna składa´c si˛e z wielokrotnie powtórzonego znaku ^, tyle razy, ile jest
-- w sumie znaków od / do \ w ostatniej linijce cz˛e´sci górnej.
-- • Pien powinien składa ´ ´c si˛e ze spacji i dokładnie dwóch znaków | w taki sposób, ˙ze znaki |
-- powinny by´c umieszczone dokładnie pod szczytem choinki (znakami /\).
-- Rozmiar choinki to liczba linijek jej cz˛e´sci górnej (która — zgodnie z zasadami powy˙zej —
-- implikuje szeroko´s´c cz˛e´sci dolnej i poło˙zenie pnia). Przykładowe choinki dla liczb 1 i 3:
-- /\
-- ^^
-- ||
--   /\
--  /  \
-- /    \
-- ^^^^^^
--   ||

drawTree :: Int -> IO ()
drawTree size
  | size <= 0 = putStrLn "Wielkość choinki musi być liczbą naturalną dodatnią."
  | otherwise = do
    drawUpperPart size
    drawLowerPart size
    drawTrunk size

drawUpperPart :: Int -> IO ()
drawUpperPart size = mapM_ putStrLn upperPartLines
  where
    upperPartLines = [spaces ++ "/" ++ replicate i '\\' ++ "\\" | i <- [1,3..2 * size - 1]]
    spaces = replicate (size - 1) ' '

drawLowerPart :: Int -> IO ()
drawLowerPart size = putStrLn (replicate (2 * size - 1) '^')

drawTrunk :: Int -> IO ()
drawTrunk size = putStrLn trunkLine
  where
    trunkLine = replicate (size - 1) ' ' ++ "||"

main :: IO ()
main = do
  putStrLn "Podaj wielkość choinki:"
  userInput <- getLine
  let size = read userInput :: Int
  drawTree size