addTwoNumbers :: Int -> Int -> Int
addTwoNumbers = (+)

main :: IO()
main = do
    x <- readLn
    y <- readLn
    putStrLn $ show $ addTwoNumbers x y
