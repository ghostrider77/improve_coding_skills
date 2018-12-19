import Data.Char (ord)
import Data.Vector (Vector(..), fromList, generate, (!))
import qualified Data.Vector as V
import System.Random (randomR, mkStdGen)


polynomialHashing :: Vector Char -> Integer -> Integer -> Int
polynomialHashing s prime x = fromInteger $ foldr (\chr acc -> (acc * x + toInteger (ord chr)) `mod` prime) 0 s


calcPowerOfX :: Integer -> Integer -> Int -> Integer
calcPowerOfX x prime exponent = foldl (\acc _ -> (acc * x) `mod` prime) 1 [1..exponent]


substringHashes :: Vector Char -> Int -> Int -> Integer -> Integer -> Vector Int
substringHashes text textLength patternLength prime x =
    let len = textLength - patternLength + 1
        xPower = calcPowerOfX x prime patternLength
        hashValues = generate len calcHashes
        calcHashes :: Int -> Int
        calcHashes ix =
            if ix == len - 1 then
                let lastSubstring = V.slice (len - 1) patternLength text in polynomialHashing lastSubstring prime x
            else
                let hashed = x * toInteger (hashValues ! (ix + 1))
                               + toInteger (ord (text ! ix)) - xPower * toInteger (ord (text ! (ix + patternLength)))
                in fromInteger $ hashed `mod` prime
    in hashValues


getMatchingIndices :: Vector Char -> Int -> Vector Char -> Int -> Int -> Vector Int -> [Int]
getMatchingIndices text textLength pattern patternLength patternHash hashValues =
    let len = textLength - patternLength + 1
        go :: Int -> [Int] -> [Int]
        go ix acc
            | ix == len = reverse acc
            | patternHash == hashValues ! ix && pattern == V.slice ix patternLength text = go (ix + 1) (ix : acc)
            | otherwise = go (ix + 1) acc
    in go 0 []


findPatternInText :: Vector Char -> Vector Char -> Integer -> [Int]
findPatternInText text pattern prime =
    let x = fst $ randomR (1, prime - 1) (mkStdGen 2112)
        patternLength = V.length pattern
        textLength = V.length text
        patternHash = polynomialHashing pattern prime x
        hashValues = substringHashes text textLength patternLength prime x
    in getMatchingIndices text textLength pattern patternLength patternHash hashValues


main :: IO()
main = do
    pattern <- fmap fromList getLine
    text <- fmap fromList getLine
    let prime = 1000000007
    let indices = findPatternInText text pattern prime
    putStrLn $ unwords $ map show indices
