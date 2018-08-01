import Control.Monad (replicateM)
import Data.Array (Array(..), listArray, (!), range)


calcLevenshteinDistance :: String -> String -> Int
calcLevenshteinDistance s1 s2 = editDistance n m
    where
        (n, m) = (length s1, length s2)
        (a1, a2) = (listArray (1, n) s1, listArray (1, m) s2)
        editDistance :: Int -> Int -> Int
        editDistance ix 0 = ix
        editDistance 0 jy = jy
        editDistance ix jy =
            let matchingScore = if (a1 ! ix == a2 ! jy) then 0 else 1
                deletion = editDistanceValues ! (ix - 1, jy) + 1
                insertion = editDistanceValues ! (ix, jy - 1) + 1
                matching = editDistanceValues ! (ix - 1, jy - 1) + matchingScore
            in minimum [deletion, insertion, matching]
        bounds = ((0, 0), (n, m))
        editDistanceValues = listArray bounds [editDistance ix jy | (ix, jy) <- range bounds]


main :: IO()
main = do
    [s1, s2] <- replicateM 2 getLine
    print $ calcLevenshteinDistance s1 s2
