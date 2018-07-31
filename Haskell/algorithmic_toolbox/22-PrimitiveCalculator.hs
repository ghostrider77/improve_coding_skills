import Data.Vector (Vector(..), generate, (!))
import qualified Data.Vector as V

nominators :: [Int]
nominators = [2, 3]


backtrackCalculation :: Vector Int -> Int -> [Int]
backtrackCalculation backtrack n = go (n - 1) [n]
    where
        go :: Int -> [Int] -> [Int]
        go k path
            | k <= 1 = path
            | otherwise = let m = backtrack ! k in go m ((m + 1):path)


findPreviousMinimum :: Vector (Int, Int) -> Int -> (Int, Int)
findPreviousMinimum lookupArrays k =
    foldl checkForShorterCalculation (fst $ lookupArrays ! position, position) nominators
        where
            position = k - 2
            checkForShorterCalculation :: (Int, Int) -> Int -> (Int, Int)
            checkForShorterCalculation acc nom
                | k `mod` nom == 0 =
                    let pos = k `div` nom - 1
                        nrOps = fst $ lookupArrays ! pos
                    in if (nrOps < fst acc) then (nrOps, pos) else acc
                | otherwise = acc


runCalculator :: Int -> [Int]
runCalculator n = if (n == 1) then [n] else backtrackCalculation backtrack n
    where
        lookupArrays = generate n updateLookupArrays
        backtrack = snd $ V.unzip lookupArrays
        updateLookupArrays :: Int -> (Int, Int)
        updateLookupArrays k = if (k == 0)  then (0, 0) else (previousMinimum + 1, position)
            where (previousMinimum, position) = findPreviousMinimum lookupArrays (k + 1)


main :: IO()
main = do
    n <- readLn
    let result = runCalculator n
    print $ length result - 1
    putStrLn $ unwords $ map show result
