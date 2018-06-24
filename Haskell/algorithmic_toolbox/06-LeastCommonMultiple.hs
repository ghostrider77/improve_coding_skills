
convertToIntList :: String -> [Int]
convertToIntList = map read . words


calcGCD :: Int -> Int -> Int
calcGCD a b = if b == 0 then a else calcGCD b (a `mod` b)


calcLCM :: Int -> Int -> Integer
calcLCM a b = toInteger (a `div` gcd') * toInteger b
    where gcd' = calcGCD a b


main :: IO()
main = do
    line <- getLine
    let [a, b] = convertToIntList line
    let result = calcLCM a b
    print result
