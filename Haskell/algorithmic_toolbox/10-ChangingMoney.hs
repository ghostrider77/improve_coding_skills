
coins :: [Int]
coins = [10, 5, 1]


calcMinimumNumberOfCoins :: Int -> Int
calcMinimumNumberOfCoins amount = go amount coins 0
    where
        go :: Int -> [Int] -> Int -> Int
        go _ [] nrChanges = nrChanges
        go amount' (coin:remainingCoins) nrChanges =
            go (amount' `mod` coin) remainingCoins (nrChanges + amount' `div` coin)


main :: IO()
main = do
    amount <- readLn
    print $ calcMinimumNumberOfCoins amount
