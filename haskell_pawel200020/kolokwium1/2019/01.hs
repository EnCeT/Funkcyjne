sumOfDigits:: Int -> Int
sumOfDigits 0 = 0
sumOfDigits n = n`mod`10 + sumOfDigits (n`div`10)

getLatestSumOfDigits:: Int -> Int
getLatestSumOfDigits n = if(n `div` 10) == 0 then n else sumOfDigits(last (takeWhile (\x->(x`div`10)/=0) (iterate sumOfDigits n)))

sevens:: Int-> [Int]
sevens 1 = [7]
sevens n = take n [x|x<-[1..],getLatestSumOfDigits x ==7]
