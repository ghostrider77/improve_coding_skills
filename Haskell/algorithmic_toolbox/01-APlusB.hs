addTwoNumbers :: Int -> Int -> Int
addTwoNumbers = (+)

main :: IO()
main = do
    x <- readLn
    y <- readLn
    print $ addTwoNumbers x y
