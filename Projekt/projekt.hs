import System.IO
import System.Environment
combinations :: Int -> [[a]] -> [[[a]]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs

podlisty :: Int -> [Int] -> [[Int]]
podlisty 0 _ = [[]]
podlisty _ [] = []
podlisty n (x:xs) = map (x:) (podlisty (n - 1) xs) ++ podlisty n xs

kelements :: Int -> Int -> Int -> Int -> [[Int]]
kelements n k rt t = podlisty k [1..n]

numerator :: Float -> Float -> Float
numerator n 2 = 1
numerator n t = if t - 1 == 2 then n - 2 else (n - t + 1) * (numerator n (t - 1))

denominator :: Float -> Float -> Float
denominator k 2 = 1
denominator k t = if t - 1 == 2 then k - 2 else (k - t + 1) * (denominator k (t - 1))

tocommondesigns :: Float -> Float -> Float -> Float -> Float
tocommondesigns n k rt t = rt * (numerator n t) / (denominator k t)

bx :: Float -> Float -> Float -> Float -> Float
bx n k rt t = rt * ((numerator n t)* (n - 1) * n) / ((denominator k t) * (k - 1) * k)

rx :: Float -> Float -> Float -> Float -> Float
rx n k rt t = rt * ((numerator n t)*(n - 1)) / ((denominator k t) * (k - 1))

isitsubset :: [Int] -> [Int] -> Bool
isitsubset [] _ = True
isitsubset (x:xs) y = if elem x y then isitsubset xs y else False

conditionchecker :: [Int] -> [[Int]] -> Int -> Bool
conditionchecker _ [] 0 = True
conditionchecker _ [] _ = False
conditionchecker x (y:ys) rt =  if isitsubset x y then conditionchecker x ys (rt - 1) else conditionchecker x ys rt

designschecker :: [[Int]] -> [[Int]] -> Int -> Bool
designschecker [] _ _ = True
designschecker (x:xs) y rt = conditionchecker x y rt && designschecker xs y rt 

writeToFile :: FilePath -> [[[Int]]] -> Int -> Int -> Int -> Int -> IO ()
writeToFile filePath list n k rt t = do
    handle <- openFile filePath WriteMode
    if t == 1 then
        hPutStr handle ("r = " ++ show (t) ++ "\n" ++ "b = " ++ show (round (fromIntegral (n * rt) / fromIntegral k)) ++ "\n" ++ "rt = " ++ show (rt) ++ "\n")
    else 
        hPutStr handle ("r = " ++ show (rx (fromIntegral n) (fromIntegral k) (fromIntegral rt) (fromIntegral t)) ++ "\n" ++ "b = " ++ show (bx (fromIntegral n) (fromIntegral k) (fromIntegral rt) (fromIntegral t)) ++ "\n" ++ "rt = " ++ show (tocommondesigns (fromIntegral n) (fromIntegral k) (fromIntegral rt) (fromIntegral t)) ++ "\n")
    if null (concat list) then 
        hPutStr handle "Brak konfiguracji o wskazanych parametrach"
    else 
        mapM_ (write handle) list

    hClose handle        

write :: Handle -> [[Int]] -> IO ()
write handle (l:list) = do
    hPutStr handle (show l)
    hPutStr handle "\n"
    if null list then hPutStr handle "----------\n"
    else write handle list

-- designschecker1 :: [Int] -> [[Int]] -> Int -> Bool

isInt x = x == fromInteger (round x)

designs1 :: Int -> Int -> Int -> Int -> [[[Int]]]
designs1 n k rt t = if b == 0 then [[]] else filter (\x -> designschecker kelem x l) de
    where 
        r = t
        b = if isInt (fromIntegral (n * rt) / fromIntegral k) then fromIntegral (n * rt) / fromIntegral k else 0
        l = rt
        kelem = [[x] | x <- [1..n]]
        de = combinations (round b) (kelements n k rt t)
    

designs :: Int -> Int -> Int -> Int -> [[[Int]]]
designs n k rt t = if t == 1 then designs1 n k rt t else if isInt r && isInt b && isInt l then filter (\x -> designschecker kelem x (round l)) de else [[]]
    where 
        r = rx (fromIntegral n) (fromIntegral k) (fromIntegral rt) (fromIntegral t)
        b = bx (fromIntegral n) (fromIntegral k) (fromIntegral rt) (fromIntegral t)
        l = tocommondesigns (fromIntegral n) (fromIntegral k) (fromIntegral rt) (fromIntegral t)
        kelem = kelements n t rt t
        de = combinations (round b) (kelements n k rt t)


main :: IO ()
main = do 

    args <- getArgs
    if length args == 4
    then do
        let [n, k, rt, t] = map read args :: [Int]
        let list = designs n k rt t
        writeToFile "fileout.txt" list n k rt t
        putStrLn "Done" 
    else putStrLn "Please provide exactly four integers as arguments."