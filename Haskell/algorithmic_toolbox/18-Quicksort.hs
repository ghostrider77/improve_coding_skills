
readInputArray :: IO [Int]
readInputArray = do
    line <- getLine
    return $ map read $ words line


quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort [x] = [x]
quicksort xs@(x:xss) = let first = filter (<x) xss
                           middle = filter (==x) xs
                           second = filter (>x) xss
                       in quicksort first ++ middle ++ quicksort second


main :: IO()
main = do
    n <- readLn :: IO Int
    lst <- readInputArray
    putStrLn $ unwords $ map show $ quicksort lst
