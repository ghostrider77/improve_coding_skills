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


preorderTraversal :: Tree -> [Int]
preorderTraversal (Tree nodes rootIx) = go [] rootIx []
        where
            go :: [Int] -> Int -> [Int] -> [Int]
            go keys nodeIndex stack
                | nodeIndex /= -1 =
                    let Node key leftIx rightIx = nodes ! nodeIndex in go (key:keys) leftIx (rightIx:stack)
                | otherwise = case stack of [] -> reverse keys
                                            ix:rest -> go keys ix rest


postorderTraversal :: Tree -> [Int]
postorderTraversal (Tree nodes rootIx) =
    let orderedIndices = findNodeOrder [rootIx] [] in map (\ix -> key $ nodes ! ix) orderedIndices
    where
        findNodeOrder :: [Int] -> [Int] -> [Int]
        findNodeOrder [] stack2 = stack2
        findNodeOrder (ix:rest) stack2 =
            if ix == -1 then findNodeOrder rest stack2
            else let Node _ leftIx rightIx = nodes ! ix in findNodeOrder (rightIx:leftIx:rest) (ix:stack2)


main :: IO()
main = do
    nrNodes <- readLn
    nodes <-readNodeInformation nrNodes
    let tree = Tree nodes 0
    let inorder = inorderTraversal tree
    let preorder = preorderTraversal tree
    let postorder = postorderTraversal tree
    print "hello"
    putStrLn $ unwords $ map show inorder
    putStrLn $ unwords $ map show preorder
    putStrLn $ unwords $ map show postorder
