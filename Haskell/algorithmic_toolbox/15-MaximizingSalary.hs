import Data.List (intercalate, sortBy)


assembleLargestNumberFromPieces :: [String] -> [String]
assembleLargestNumberFromPieces numberStrings = sortBy (flip order) numberStrings
    where
        order :: String -> String -> Ordering
        order n1 n2
            | n1 ++ n2 < n2 ++ n1 = LT
            | n1 ++ n2 > n2 ++ n1 = GT
            | otherwise = EQ


main :: IO()
main = do
    _ <- getLine
    line <- getLine
    let result = assembleLargestNumberFromPieces $ words line
    putStrLn $ intercalate "" result
