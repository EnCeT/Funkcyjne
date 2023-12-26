ciagA:: Int -> Int 
ciagA 0 = 1
ciagA n = (n-1)*(ciagB (n-1))-3*(ciagA(n-1)) 
ciagB :: Int -> Int
ciagB 0 = 1
ciagB n = 3*(ciagB(n-1))+(n-1)*(n-1)*ciagA(n-1)-(n-1)*(n-1)

sumList:: Int ->Int->Int-> [Int] -> Int
sumList n begin count  (l:list) = if(begin+l)>n then count else sumList n  (begin+l) (count+1) list

seqIndex:: Int-> Int 
seqIndex n = sumList n 1 1 (map ciagA [1..])