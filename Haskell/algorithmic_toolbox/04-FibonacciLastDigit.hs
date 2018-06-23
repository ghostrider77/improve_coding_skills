
modulus :: Int
modulus = 10

calcFibonacciLastDigit :: Int -> Int
calcFibonacciLastDigit n = fst $ foldr (\_ (a, b) -> (b, (a + b) `mod` modulus)) (0, 1) [1..n]

main :: IO()
main = do
    n <- readLn
    putStrLn $ show $ calcFibonacciLastDigit n
