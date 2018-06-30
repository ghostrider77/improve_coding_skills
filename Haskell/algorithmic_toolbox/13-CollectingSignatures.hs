import Control.Monad (replicateM)
import GHC.Exts (sortWith)

data Segment = Segment {left :: Int, right :: Int}


convertToIntList :: String -> [Int]
convertToIntList = map read . words


readIntoSegment :: [Int] -> Segment
readIntoSegment [a, b] = Segment a b


readInput :: Int -> IO [Segment]
readInput n = do
    lines' <- replicateM n getLine
    return $ map (readIntoSegment . convertToIntList) lines'


calcPointsCoveringSegments :: [Segment] -> [Int]
calcPointsCoveringSegments segments = go (sortWith right segments) []
    where
        go :: [Segment] -> [Int] -> [Int]
        go [] points = points
        go ((Segment _ b):rest) points = go (filter (\segment -> left segment > b) rest) (b:points)


main :: IO()
main = do
    nrSegments <- readLn
    segments <- readInput nrSegments
    let covering = calcPointsCoveringSegments segments
    print $ length covering
    putStrLn $ unwords $ map show covering
