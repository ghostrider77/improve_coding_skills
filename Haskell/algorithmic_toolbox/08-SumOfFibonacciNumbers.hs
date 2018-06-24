
modulus :: Int
modulus = 10


calcPisanoPeriod :: Int -> Integer
calcPisanoPeriod modulus' = go 1 1 1
    where go :: Int -> Int -> Integer -> Integer
          go a b p
              | a == 0 && b == 1 = p
              | otherwise = go b ((a + b) `mod` modulus') p + 1


calcFibonacciModulo :: Integer -> Int -> Int
calcFibonacciModulo n modulus' = fst $ foldr (\_ (a, b) -> (b, (a + b) `mod` modulus')) (0, 1) [1..n]


calcLastDigitOfTheSumOfFibonacciNumbers :: Integer -> Int
calcLastDigitOfTheSumOfFibonacciNumbers n = (calcFibonacciModulo ((n + 2) `mod` p) modulus - 1) `mod` modulus
    where p = calcPisanoPeriod modulus


main :: IO()
main = do
    line <- getLine
    let n = read line
    print $ calcLastDigitOfTheSumOfFibonacciNumbers n
