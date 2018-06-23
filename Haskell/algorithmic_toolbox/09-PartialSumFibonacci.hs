
modulus :: Int
modulus = 10


convertToIntegerList :: String -> [Integer]
convertToIntegerList = map read . words


calcPisanoPeriod :: Int -> Integer
calcPisanoPeriod modulus' = go 1 1 1
    where go :: Int -> Int -> Integer -> Integer
          go a b p
              | a == 0 && b == 1 = p
              | otherwise = go b ((a + b) `mod` modulus') p + 1


calcFibonacciModulo :: Integer -> Int -> Int
calcFibonacciModulo n modulus' = fst $ foldr (\_ (a, b) -> (b, (a + b) `mod` modulus')) (0, 1) [1..n]


calcLastDigitOfPartialSum :: Integer -> Integer -> Int
calcLastDigitOfPartialSum m n =
    let p = calcPisanoPeriod modulus
        lastDigitOfPrefixSum = (calcFibonacciModulo ((m + 1) `mod` p) modulus - 1) `mod` modulus
        lastDigitOfFullSum = (calcFibonacciModulo ((n + 2) `mod` p) modulus - 1) `mod` modulus
    in (lastDigitOfFullSum - lastDigitOfPrefixSum) `mod` modulus


main :: IO()
main = do
    line <- getLine
    let [m, n] = convertToIntegerList line
    putStrLn $ show $ calcLastDigitOfPartialSum m n
