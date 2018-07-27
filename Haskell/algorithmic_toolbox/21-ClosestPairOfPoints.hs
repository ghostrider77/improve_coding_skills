import Control.Monad (replicateM)
import GHC.Exts (sortWith)

data Point = Point {x :: Double, y :: Double}

bruteForceSize :: Int
bruteForceSize = 3


infinity :: Double
infinity = read "Infinity"


convertToDoubleList :: String -> [Double]
convertToDoubleList = map read . words


readPoints :: [String] -> [Point]
readPoints lines' = map (createPoint . convertToDoubleList) lines'
    where createPoint :: [Double] -> Point
          createPoint [u, v] = Point u v


distance :: Point -> Point -> Double
distance (Point x' y') (Point x'' y'') = sqrt ((x' - x'')^2 + (y' - y'')^2)


calcSmallestPairwiseDistance :: [Point] -> Double -> Int -> Double
calcSmallestPairwiseDistance points minDistance nrPointsCompareWith = foldl calcDistances minDistance (zip points [0..])
    where
        calcDistances :: Double -> (Point, Int) -> Double
        calcDistances currentMinDistance (p, ix) = foldl updateMinDistance currentMinDistance slice
            where
                slice = take nrPointsCompareWith (drop (ix + 1) points)
                updateMinDistance :: Double -> Point -> Double
                updateMinDistance acc q = if dist < acc then dist else acc
                    where dist = distance p q


calcMinimumDistanceInStripe :: [Point] -> [Point] -> Double -> Double -> Double
calcMinimumDistanceInStripe first second median delta = calcSmallestPairwiseDistance (sortWith y stripe) delta 7
    where inStripe :: Point -> Bool
          inStripe (Point x' _) = abs (x' - median) <= delta
          stripe = (filter inStripe first) ++ (filter inStripe second)


findClosestPoints :: [Point] -> Int -> Double
findClosestPoints sortedPoints n
    | n <= bruteForceSize = calcSmallestPairwiseDistance sortedPoints infinity (bruteForceSize - 1)
    | otherwise = let middleIx = n `div` 2
                      median = x $ sortedPoints !! middleIx
                      (first, second) = splitAt middleIx sortedPoints
                      delta1 = findClosestPoints first middleIx
                      delta2 = findClosestPoints second (n - middleIx)
                      delta = min delta1 delta2
                  in if abs delta < 1e-14 then 0.0 else calcMinimumDistanceInStripe first second median delta


findClosestPairOfPoints :: [Point] -> Int -> Double
findClosestPairOfPoints points n = findClosestPoints sortedPoints n
    where sortedPoints = sortWith x points


main :: IO()
main = do
    n <- readLn
    lines' <- replicateM n getLine
    let points = readPoints lines'
    let dist = findClosestPairOfPoints points n
    print dist
