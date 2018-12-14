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
        go :: [Int] -> Int -> [Int] -> [Int]
        go nodeIndices nodeIx stack
            | nodeIx /= -1 = let Node _ leftIx _ = nodes ! nodeIx in go nodeIndices leftIx (nodeIx:stack)
            | otherwise = case stack of [] -> reverse nodeIndices
                                        ix:rest -> let Node _ _ rightIx = nodes ! ix in go (ix:nodeIndices) rightIx rest


isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (a:b:rest) = if a <= b then isSorted (b:rest) else False


areDuplicatesInRightSubtree :: Vector Int -> [Int] -> Tree -> Bool
areDuplicatesInRightSubtree keys indicesOfNodes (Tree nodes _) = go $ zip indicesOfNodes [0..]
    where
        go :: [(Int, Int)] -> Bool
        go [] = True
        go ((indexOfNode, ix):ixss) =
            let Node key leftIx _ = nodes ! indexOfNode in
                if leftIx == -1 then go ixss
                else let keyToTheLeft = keys ! (ix - 1) in if keyToTheLeft == key then False else go ixss


isValidBinarySearchTree :: Tree -> Int -> Bool
isValidBinarySearchTree tree @ (Tree nodes _) nrNodes
    | nrNodes <= 1 = True
    | otherwise =
        let inorderIndicesOfNodes = inorderTraversal tree
            keys = map (\ix -> key $ nodes ! ix) inorderIndicesOfNodes
        in isSorted keys && areDuplicatesInRightSubtree (fromList keys) inorderIndicesOfNodes tree


main :: IO()
main = do
    nrNodes <- readLn
    nodes <-readNodeInformation nrNodes
    let tree = Tree nodes 0
    let verdict = isValidBinarySearchTree tree nrNodes
    print $ if verdict then "CORRECT" else "INCORRECT"
