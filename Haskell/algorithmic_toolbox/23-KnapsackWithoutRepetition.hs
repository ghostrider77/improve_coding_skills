import Control.Monad (replicateM)
import Data.Array (listArray, (!), range)

convertToIntList :: String -> [Int]
convertToIntList = map read . words


readInput :: Int -> IO [[Int]]
readInput n = do
    lines' <- replicateM n getLine
    return $ map convertToIntList lines'


solveKnapsackProblem :: [Int] -> Int -> Int -> Int
solveKnapsackProblem ws n capacity = knapsack capacity n
    where
        weights = listArray (1, n) ws
        knapsack :: Int -> Int -> Int
        knapsack c ix
            | c == 0 || ix == 0 = 0
            | c < weight = solution ! (c, ix - 1)
            | otherwise = max (solution ! (c - weight, ix - 1) + weight) (solution ! (c, ix - 1))
                where
                    weight = weights ! ix
                    bounds = ((0, 0), (capacity, n))
                    solution = listArray bounds [knapsack c k | (c, k) <- range bounds]


main :: IO()
main = do
    [[capacity, nrWeights], weights] <- readInput 2
    print $ solveKnapsackProblem weights nrWeights capacity
