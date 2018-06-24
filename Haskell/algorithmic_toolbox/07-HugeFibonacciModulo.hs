
convertToIntegerList :: String -> [Integer]
convertToIntegerList = map read . words


calcPisanoPeriod :: Int -> Integer
calcPisanoPeriod modulus = go 1 1 1
    where go :: Int -> Int -> Integer -> Integer
          go a b p
              | a == 0 && b == 1 = p
              | otherwise = go b ((a + b) `mod` modulus) p + 1


calcFibonacciModulo :: Integer -> Int -> Int
calcFibonacciModulo n modulus = fst $ foldr (\_ (a, b) -> (b, (a + b) `mod` modulus)) (0, 1) [1..n]


calcHugeFibonacciModulo :: Integer -> Int -> Int
calcHugeFibonacciModulo n modulus = calcFibonacciModulo (n `mod` p) modulus
    where p = calcPisanoPeriod modulus


main :: IO()
main = do
    line <- getLine
    let [n, modulus] = convertToIntegerList line
    print $ calcHugeFibonacciModulo n (fromIntegral modulus)
