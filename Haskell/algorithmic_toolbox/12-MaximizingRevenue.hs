import Control.Monad (replicateM)
import Data.List (sort)


convertToIntList :: String -> [Int]
convertToIntList = map read . words


readInput :: Int -> IO [[Int]]
readInput n = do
    lines' <- replicateM n getLine
    return $ map convertToIntList lines'


calcMaximalRevenue :: [Int] -> [Int] -> Integer
calcMaximalRevenue xs ys = foldr (\(x, y) acc -> acc + (toInteger x) * (toInteger y)) 0 zipped
    where zipped = zip (sort xs) (sort ys)


main :: IO()
main = do
    _ <- getLine
    [profitPerClick, averageClickPerDay] <- readInput 2
    print $ calcMaximalRevenue profitPerClick  averageClickPerDay
