import Control.Monad (replicateM)
import GHC.Exts (Down(..), sortWith)

data Item = Item {value :: Int, weight :: Int}


convertToIntList :: String -> [Int]
convertToIntList = map read . words


readItems :: [String] -> [Item]
readItems lines' = map (pairToItem . convertToIntList) lines'
    where
        pairToItem :: [Int] -> Item
        pairToItem [value', weight'] = Item value' weight'


solveFractionalKnapsack :: [Item] -> Int -> Double
solveFractionalKnapsack items capacity = go capacity sortedItems 0
    where
        sortedItems = sortWith (\(Item value' weight') -> Down (fromIntegral value' / fromIntegral weight')) items
        go :: Int -> [Item] -> Double -> Double
        go 0 _ totalValue = totalValue
        go _ [] totalValue = totalValue
        go capacity' ((Item v w):xss) totalValue = go (capacity' - usedAmount) xss increasedValue
            where
                usedAmount = min capacity' w
                increasedValue = (totalValue + fromIntegral (usedAmount * v) / fromIntegral w)


main :: IO()
main = do
    line <- getLine
    let [nrItems, capacity] = convertToIntList line
    lines' <- replicateM nrItems getLine
    let items = readItems lines'
    print $ solveFractionalKnapsack items capacity
