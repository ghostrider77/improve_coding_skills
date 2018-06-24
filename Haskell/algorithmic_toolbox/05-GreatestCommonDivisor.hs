
convertToIntList :: String -> [Int]
convertToIntList = map read . words


calcGCD :: Int -> Int -> Int
calcGCD a b = if b == 0 then a else calcGCD b (a `mod` b)


main :: IO()
main = do
    line <- getLine
    let [a, b] = convertToIntList line
    let result = calcGCD a b
    print result
