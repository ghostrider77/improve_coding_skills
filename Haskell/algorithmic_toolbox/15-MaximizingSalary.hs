import Data.List (intercalate, sortBy)
import Data.Ord (Down(..), comparing)

data Number = Number String

instance Eq Number where
    (Number n1) == (Number n2) = n1 ++ n2 == n2 ++ n1

instance Ord Number where
    (Number n1) <= (Number n2) = n1 ++ n2 <= n2 ++ n1

instance Show Number where
    show (Number n) = n


assembleLargestNumberFromPieces :: [Number] -> [String]
assembleLargestNumberFromPieces numberStrings = map show $ sortBy (comparing Down) numberStrings


main :: IO()
main = do
    _ <- getLine
    line <- getLine
    let result = assembleLargestNumberFromPieces $ map Number $ words line
    putStrLn $ intercalate "" result
