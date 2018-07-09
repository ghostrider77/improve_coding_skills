import Data.List (group, sort)

convertToIntList :: String -> [Int]
convertToIntList = map read . words


hasMajorityElem :: [Int] -> Int -> Bool
hasMajorityElem lst n = any (> n `div` 2) (map length $ group $ sort lst)


main :: IO()
main = do
    n <- readLn
    line <- getLine
    let list = convertToIntList line
    print $ if (hasMajorityElem list n) then 1 else 0
