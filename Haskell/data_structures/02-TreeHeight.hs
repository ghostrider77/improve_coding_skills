import Data.Map.Strict (Map(..))
import Data.Vector (Vector(..), (!), generate)
import Data.Set (Set(..))
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Node = Node { key :: Int, children :: Set Int }
data Tree = Tree { root :: Node, nodes :: Vector Node }


convertToIntList :: String -> [Int]
convertToIntList = map read . words


getIndexOfRoot :: Map Int (Set Int) -> Int
getIndexOfRoot childrenOfNodes = head $ S.elems $ childrenOfNodes M.! (-1)


calcChildrenFromParents :: Int -> [Int] -> Map Int (Set Int)
calcChildrenFromParents nrNodes parentsOfNodes = foldl update M.empty parentsWithIndices
    where
        parentsWithIndices = zip parentsOfNodes [0..]
        update :: Map Int (Set Int) -> (Int, Int) -> Map Int (Set Int)
        update table (parentId, nodeId) =
            let children = M.findWithDefault S.empty parentId table
                children' = S.insert nodeId children
            in M.insert parentId children' table


buildTree :: Int -> [Int] -> Tree
buildTree nrNodes parentsOfNodes =
    let childrenOfNodes = calcChildrenFromParents nrNodes parentsOfNodes
        nodes = generate nrNodes (\key -> Node key (M.findWithDefault S.empty key childrenOfNodes))
        indexOfRoot = getIndexOfRoot childrenOfNodes
    in Tree (nodes ! indexOfRoot) nodes


getChildrenOfNodes :: Set Int -> Vector Node -> Set Int
getChildrenOfNodes keys nodes = S.foldl (\acc key -> S.union acc (extractChildren key)) S.empty keys
    where
        extractChildren :: Int -> Set Int
        extractChildren key = children $ nodes ! key


calcTreeDepth :: Tree -> Int
calcTreeDepth (Tree root nodes) = go 0 (S.singleton $ key root)
    where
        go :: Int -> Set Int -> Int
        go depth keys
            | S.null keys = depth
            | otherwise = let nextKeys = getChildrenOfNodes keys nodes in go (depth + 1) nextKeys


main :: IO()
main = do
    nrNodes <- readLn
    parentsOfNodes <- fmap convertToIntList getLine
    let tree = buildTree nrNodes parentsOfNodes
    let depth = calcTreeDepth tree
    print depth
