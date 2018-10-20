import Control.Monad (replicateM)
import Data.Vector (Vector, (!), fromList)

convertToIntList :: String -> [Int]
convertToIntList = map read . words


readInput :: Int -> IO [[Int]]
readInput n = do
    lines' <- replicateM n getLine
    return $ map convertToIntList lines'


findSingleElem :: Vector Int -> Int -> Int -> Int
findSingleElem lst n x = binarySearch 0 (n - 1)
    where
        binarySearch :: Int -> Int -> Int
        binarySearch left right
            | left > right = -1
            | otherwise =
                let middleIx = (left + right) `div` 2
                    middleElem = lst ! middleIx
                in  if (middleElem == x) then middleIx
                    else
                        if (middleElem < x) then binarySearch (middleIx + 1) right
                        else binarySearch left (middleIx - 1)


findElems :: Vector Int -> Int -> [Int] -> [Int]
findElems vec n queries = map (findSingleElem vec n) queries


main :: IO()
main = do
    [inputList, queryList] <- readInput 2
    print $ findElems (fromList $ tail inputList) (head inputList) (tail queryList)
