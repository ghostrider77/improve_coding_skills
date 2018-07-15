import Data.List (sort)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed (Vector(..), fromList)
import Control.Monad (replicateM)

convertToIntList :: String -> [Int]
convertToIntList = map read . words


readSegmentEndPoints :: Int -> IO ([Int], [Int])
readSegmentEndPoints n = do
    lines' <- replicateM n getLine
    let segments = map ((\[a, b] -> (a, b)) . convertToIntList) lines'
    return $ unzip segments


getNumberOfSuitableEndpoints :: Vector Int -> Int -> Int -> Int
getNumberOfSuitableEndpoints endPoints size point
    | V.last endPoints <= point = size
    | otherwise = binarySearch 0 (size - 1)
        where
            binarySearch :: Int -> Int -> Int
            binarySearch a b
                | a == b = a
                | otherwise =
                    let mid = (a + b) `div` 2
                    in if (endPoints V.! mid <= point) then binarySearch (mid + 1) b else binarySearch a mid


nrIntersectingSegments :: Vector Int -> Vector Int -> Int -> Int -> Int
nrIntersectingSegments sortedLeft sortedNegatedRight nrSegments point =
    let nrGoodLeftEnds = getNumberOfSuitableEndpoints sortedLeft nrSegments point
        nrGoodRightEnds = getNumberOfSuitableEndpoints sortedNegatedRight nrSegments (-point)
    in nrGoodLeftEnds + nrGoodRightEnds - nrSegments


numberOfSegmentsContainingPoints :: [Int] -> [Int] -> Int -> [Int] -> [Int]
numberOfSegmentsContainingPoints leftEndPoints rightEndPoints nrSegments points =
    let sortedLeft = fromList $ sort leftEndPoints
        sortedNegatedRight = fromList $ sort $ map negate rightEndPoints
    in map (nrIntersectingSegments sortedLeft sortedNegatedRight nrSegments) points


main :: IO()
main = do
    params <- getLine
    let [nrSegments, _] = convertToIntList params
    (leftEndPoints, rightEndPoints) <- readSegmentEndPoints nrSegments
    line <- getLine
    let points = convertToIntList line
    let result = numberOfSegmentsContainingPoints leftEndPoints rightEndPoints nrSegments points
    putStrLn $ unwords $ map show result
