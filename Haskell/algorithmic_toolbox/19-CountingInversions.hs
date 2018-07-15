
convertToIntList :: String -> [Int]
convertToIntList = map read . words


mergeSortedArrays :: [Int] -> [Int] -> Int -> Int -> Integer -> ([Int], Integer)
mergeSortedArrays first second length1 length2 inversions = merge first length1 second [] inversions
    where
        merge :: [Int] -> Int -> [Int] -> [Int] -> Integer -> ([Int], Integer)
        merge xs xsLength ys acc totalInversions =
            case (xs, ys) of ([], []) -> (reverse acc, totalInversions)
                             ([], (y:yss)) -> merge [] xsLength yss (y:acc) totalInversions
                             ((x:xss), []) -> merge xss (xsLength - 1) [] (x:acc) totalInversions
                             ((x:xss, y:yss)) ->
                                 if (x <= y)
                                    then merge xss (xsLength - 1) ys (x:acc) totalInversions
                                    else merge xs xsLength yss (y:acc) (totalInversions + fromIntegral xsLength)


countInversions :: [Int] -> Int -> ([Int], Integer)
countInversions lst n
    | n <= 1 = (lst, 0)
    | otherwise =
        let middle = n `div` 2
            (first, second) = splitAt middle lst
            (length1, length2) = (middle, n - middle)
            (sortedFirst, inversionsInFirst) = countInversions first length1
            (sortedSecond, inversionsInSecond) = countInversions second length2
        in mergeSortedArrays sortedFirst sortedSecond length1 length2 (inversionsInFirst + inversionsInSecond)


main :: IO()
main = do
    n <- readLn
    line <- getLine
    let lst = convertToIntList line
    let (_, nrInversions) = countInversions lst n
    print nrInversions
