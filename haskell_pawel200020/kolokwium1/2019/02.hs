func :: [String] -> [String]
func [] = []
func x = map (\x -> (x ++ "a")) x ++ map (\x -> (x ++ "b")) x

func2 :: Int -> [String] -> [String]
func2 1 y = y
func2 n y = func2 (n - 1) (func y)

allStrings :: Int -> [String]
allStrings 0 = []
allStrings n = func2 n ["a", "b"]

bp :: Int-> [String]
bp n = filter (\x -> x == (reverse x)) (allStrings n)