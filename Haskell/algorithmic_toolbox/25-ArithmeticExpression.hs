import Data.List (partition)
import Data.Char (isDigit, digitToInt)
import Data.Array (Array(..), listArray, (!), range)
import Data.Map.Strict (Map(..), fromList)
import qualified Data.Map.Strict as Map

operationsDict :: Map Char (Int -> Int -> Int)
operationsDict = fromList [('+', (+)), ('*', (*)), ('-', (-))]


readInputData :: String -> (Array Int Int, Array Int Char)
readInputData line =
    let (digits, operations) = partition isDigit line
        nrDigits = length digits
        digitsArray = listArray (0, nrDigits - 1) (map digitToInt digits)
        operationsArray = listArray (0, nrDigits - 2) operations
    in (digitsArray, operationsArray)


calcMinMax :: Int -> Int -> Array Int Char -> Array (Int, Int) Int -> Array (Int, Int) Int -> (Int, Int)
calcMinMax ix jy operations minimumOfSubexpressions maximumOfSubexpressions =
    foldl calcSubExpressionValuesInRange (maxBound, minBound) [ix..(jy-1)]
    where
        calcSubExpressionValuesInRange :: (Int, Int) -> Int -> (Int, Int)
        calcSubExpressionValuesInRange acc k =
            let (subExpressionMin, subExpressionMax) = acc
                op = operationsDict Map.! (operations ! k)
                subexpressionsSplitAtK =
                    [op (maximumOfSubexpressions ! (ix, k)) (maximumOfSubexpressions ! (k + 1, jy)),
                     op (maximumOfSubexpressions ! (ix, k)) (minimumOfSubexpressions ! (k + 1, jy)),
                     op (minimumOfSubexpressions ! (ix, k)) (maximumOfSubexpressions ! (k + 1, jy)),
                     op (minimumOfSubexpressions ! (ix, k)) (minimumOfSubexpressions ! (k + 1, jy))]
                (splitMin, splitMax) = (minimum subexpressionsSplitAtK, maximum subexpressionsSplitAtK)
            in (min subExpressionMin splitMin, max subExpressionMax splitMax)


maximizeExpression :: Array Int Int -> Array Int Char -> Int
maximizeExpression digits operations = maximumOfSubexpressions ! (0, n - 1)
    where
        calcExtremeValues :: Int -> Int -> (Int, Int)
        calcExtremeValues ix jy
            | ix == jy = let digit = digits ! ix in (digit, digit)
            | otherwise = calcMinMax ix jy operations minimumOfSubexpressions  maximumOfSubexpressions
        n = length digits
        bounds = ((0, 0), (n - 1, n - 1))
        minimumOfSubexpressions = listArray bounds [fst $ calcExtremeValues i j | (i, j) <- range bounds]
        maximumOfSubexpressions = listArray bounds [snd $ calcExtremeValues i j | (i, j) <- range bounds]


main :: IO()
main = do
    line <- getLine
    let (digits, operations) = readInputData line
    print $ maximizeExpression digits operations
