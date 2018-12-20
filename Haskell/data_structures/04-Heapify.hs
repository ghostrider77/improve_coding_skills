import Control.Monad (replicateM)
import Control.Monad.ST
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Vector (Vector(..), fromList, thaw)
import qualified Data.Vector.Mutable as MV


convertToIntList :: String -> [Int]
convertToIntList = map read . words


getIndexOfParentChildrenMinimum :: PrimMonad m => MV.MVector (PrimState m) Int -> Int -> Int -> m Int
getIndexOfParentChildrenMinimum arr parentIndex size = do
    let leftChildIx = 2 * parentIndex + 1
    let rightChildIx = leftChildIx + 1
    minIndex <- if leftChildIx < size then do
                    v1 <- MV.read arr leftChildIx
                    v2 <- MV.read arr parentIndex
                    return $ if v1 < v2 then leftChildIx else parentIndex
                else return parentIndex
    if rightChildIx < size then do
        v1 <- MV.read arr rightChildIx
        v2 <- MV.read arr minIndex
        return $ if v1 < v2 then rightChildIx else minIndex
    else return minIndex


siftDown :: PrimMonad m => MV.MVector (PrimState m) Int -> Int -> Int -> m [(Int, Int)]
siftDown arr parentIndex n = do
    let go currentParentIx currentMinIx swaps
            | currentMinIx == currentParentIx = return swaps
            | otherwise = do
                MV.swap arr currentMinIx currentParentIx
                let nextParentIx = currentMinIx
                nextMinIx <- getIndexOfParentChildrenMinimum arr nextParentIx n
                go nextParentIx nextMinIx ((currentParentIx, currentMinIx) : swaps)
    minIndex <- getIndexOfParentChildrenMinimum arr parentIndex n
    go parentIndex minIndex []


createHeap :: PrimMonad m => MV.MVector (PrimState m) Int -> Int -> [(Int, Int)] -> Int -> m [(Int, Int)]
createHeap arr n swaps parentIndex
    | parentIndex < 0 = return $ reverse swaps
    | otherwise = do
        currentSwaps <- siftDown arr parentIndex n
        createHeap arr n (currentSwaps ++ swaps) (parentIndex - 1)


heapify :: Vector Int -> Int -> [(Int, Int)]
heapify vec n = runST $ do
    arr <- thaw vec
    createHeap arr n [] (n `div` 2 - 1)


main :: IO()
main = do
    n <- readLn
    vec <- fmap (fromList . convertToIntList) getLine
    let swaps = heapify vec n
    print $ length swaps
    mapM_ putStrLn (map (\(i, j) -> show i ++ " " ++ show j) swaps)
