
decomposeToDistinctElems :: Int -> [Int]
decomposeToDistinctElems n = go n 1 []
    where
        go :: Int -> Int -> [Int] -> [Int]
        go 0 _ distinctSummands = distinctSummands
        go number smallestSummand distinctSummands =
            let nextSummand = if (number > 2 * smallestSummand) then smallestSummand else number
            in go (number - nextSummand) (smallestSummand + 1) (nextSummand:distinctSummands)


main :: IO()
main = do
    n <- readLn
    let result = decomposeToDistinctElems n
    print $ length result
    putStrLn $ unwords $ map show result
