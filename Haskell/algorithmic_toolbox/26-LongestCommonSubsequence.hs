import Control.Monad (replicateM)
import Data.Array (Array(..), listArray, (!), range)

data Sequence = Sequence { getSeq :: Array Int Int, getLength :: Int }


convertToIntList :: String -> [Int]
convertToIntList = map read . words


readSequences :: [String] -> [Sequence]
readSequences lines' = go lines' []
    where
        go :: [String] -> [Sequence] -> [Sequence]
        go [] acc = acc
        go rawLines acc =
            let ([n, seq], rest) = splitAt 2 rawLines
                length = read n
            in go rest ((Sequence (listArray (1,length) (convertToIntList seq)) length):acc)


calcLongestCommonSubsequence :: [Sequence] -> Int
calcLongestCommonSubsequence sequences = longestPath n1 n2 n3
    where
        [s1, s2, s3] = map getSeq sequences
        [n1, n2, n3] = map getLength sequences
        longestPath :: Int -> Int -> Int -> Int
        longestPath i j k
            | i == 0 || j == 0 || k == 0 = 0
            | s1 ! i == s2 ! j && s1 ! i == s3 ! k = longestPathValues ! (i - 1, j - 1, k - 1) + 1
            | otherwise = maximum [longestPathValues ! (i - 1, j, k),
                                   longestPathValues ! (i, j - 1, k),
                                   longestPathValues ! (i, j, k - 1)]
        bounds = ((0, 0, 0), (n1, n2, n3))
        longestPathValues = listArray bounds [longestPath i j k | (i, j, k) <- range bounds]


main :: IO()
main = do
    lines' <- replicateM 6 getLine
    let sequences = readSequences lines'
    print $ calcLongestCommonSubsequence sequences
