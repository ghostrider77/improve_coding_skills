
data LargestElems = LargestElems {largest :: Int, secondLargest :: Int}


convertToIntList :: String -> [Int]
convertToIntList = map read . words


calcMaximumPairwiseProduct :: [Int] -> Integer
calcMaximumPairwiseProduct lst =
    let LargestElems a b = foldl processNextElem (LargestElems minBound minBound) lst
            where
            processNextElem acc@(LargestElems largest' secondlargest') x
                | x > largest' = LargestElems x largest'
                | x > secondlargest' = LargestElems largest' x
                | otherwise = acc
    in (toInteger a) * (toInteger b)


main :: IO()
main = do
    line <- getLine
    let list = convertToIntList line
    let result = calcMaximumPairwiseProduct list
    putStrLn $ show result
