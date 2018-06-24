
calcFibonacciNumber :: Int -> Int
calcFibonacciNumber n = fst $ foldr (\_ (a, b) -> (b, a + b)) (0, 1) [1..n]

main :: IO()
main = do
    n <- readLn
    print $ calcFibonacciNumber n
