sumOfDigitsPower:: Integer -> Integer -> Integer
sumOfDigitsPower 0 pot= 0
sumOfDigitsPower n pot = (n`mod`10)^pot + sumOfDigitsPower (n`div`10) pot

pownum :: Integer -> [Integer]
pownum pot = [x|x<-[1..], x ==(sumOfDigitsPower x pot)]