import Control.Monad (replicateM)
import Data.Vector (Vector(..), (!), fromList)

data Node = Node { key :: Int, leftIx :: Int, rightIx :: Int }
data Tree = Tree { nodes :: Vector Node, rootIx :: Int }


convertToIntList :: String -> [Int]
convertToIntList = map read . words


readNodeInformation :: Int -> IO (Vector Node)
readNodeInformation nrNodes = do
    lines' <- replicateM nrNodes getLine
    return $ fromList $ map ((\[key, left, right] -> Node key left right) . convertToIntList) lines'


inorderTraversal :: Tree -> [Int]
inorderTraversal (Tree nodes rootIx) = go [] rootIx []
    where
        go :: [Int] -> Int -> [Node] -> [Int]
        go keys nodeIx stack
            | nodeIx /= -1 = let node = nodes ! nodeIx in go keys (leftIx node) (node:stack)
            | otherwise = case stack of [] -> reverse keys
                                        (Node key _ rightIx):rest -> go (key:keys) rightIx rest


isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (a:b:rest) = if a <= b then isSorted (b:rest) else False


isValidBinarySearchTree :: Tree -> Int -> Bool
isValidBinarySearchTree tree nrNodes
    | nrNodes <= 1 = True
    | otherwise = let inorderKeys = inorderTraversal tree in isSorted inorderKeys


main :: IO()
main = do
    nrNodes <- readLn
    nodes <-readNodeInformation nrNodes
    let tree = Tree nodes 0
    let verdict = isValidBinarySearchTree tree nrNodes
    print $ if verdict then "CORRECT" else "INCORRECT"
