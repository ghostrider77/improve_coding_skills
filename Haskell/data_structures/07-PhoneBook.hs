import Control.Monad (replicateM)
import Control.Monad.ST
import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector.Mutable as MV

data Query = Addition Int String | Deletion Int | Find Int


maxSize :: Int
maxSize = 10000000


readQueries :: Int -> IO [Query]
readQueries nrQueries = do
    lines' <- replicateM nrQueries getLine
    return $ map (detectQueryType . words) lines'
    where
        detectQueryType :: [String] -> Query
        detectQueryType ["add", number, name] = Addition (read number) name
        detectQueryType ["del", number] = Deletion (read number)
        detectQueryType ["find", number] = Find (read number)


processQuery :: PrimMonad m => MV.MVector (PrimState m) (Maybe String) -> [String] -> [Query] -> m [String]
processQuery phoneBook acc [] = return $ reverse acc
processQuery phoneBook acc (Addition number name : qss) = do
    MV.write phoneBook number $ Just name
    processQuery phoneBook acc qss
processQuery phoneBook acc (Deletion number : qss) = do
    MV.write phoneBook number Nothing
    processQuery phoneBook acc qss
processQuery phoneBook acc (Find number : qss) = do
    contact <- MV.read phoneBook number
    case contact of Nothing -> processQuery phoneBook ("not found" : acc) qss
                    Just name -> processQuery phoneBook (name : acc) qss



processQueries :: [Query] -> [String]
processQueries queries = runST $ do
    phoneBook <- MV.replicate maxSize (Nothing :: Maybe String)
    processQuery phoneBook [] queries


main :: IO()
main = do
    nrQueries <- readLn
    queries <- readQueries nrQueries
    let results = processQueries queries
    mapM_ putStrLn $ results
