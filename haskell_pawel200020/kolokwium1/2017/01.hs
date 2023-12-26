help1:: Integer->Integer->[Integer]
help1 n k = if (sum (divisors k)==n+k) then  [k] ++ help1 n (k+1) else help1 n (k+1)

help2:: Integer->Integer->[Integer]
help2 n k = if k== n*n then [] else  (if (sum (divisors k)==n+k) then  [k] ++ help2 n (k+1) else help2 n (k+1))

divisors:: Integer->[Integer]
divisors n = filter (\x-> n`mod`x==0) [x|x<-[1..n]]

rd :: Integer -> [Integer]
rd 1 = help1 1 1
rd n = help2 n 1