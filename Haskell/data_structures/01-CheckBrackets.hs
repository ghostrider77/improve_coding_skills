
openingBrackets :: [Char]
openingBrackets = ['(', '[', '{']

closingBrackets :: [Char]
closingBrackets = [')', ']', '}']


data OpenedBracket = OpenedBracket { bracket :: Char, position :: Int }


doBracketsMatch :: Char -> Char -> Bool
doBracketsMatch openingBracket closingBracket =
    (openingBracket == '(' && closingBracket == ')') ||
    (openingBracket == '[' && closingBracket == ']') ||
    (openingBracket == '{' && closingBracket == '}')


retrieveFailedOpeningIndexFromStack :: [OpenedBracket] -> Maybe Int
retrieveFailedOpeningIndexFromStack [] = Nothing
retrieveFailedOpeningIndexFromStack ((OpenedBracket _ ix) : _)  = Just ix


findIndexOfNonMatchingBracket :: String -> Maybe Int
findIndexOfNonMatchingBracket string = case failedIndex of Nothing -> retrieveFailedOpeningIndexFromStack remainingStack
                                                           _       -> failedIndex
    where
        go :: [OpenedBracket] -> [(Char, Int)] -> ([OpenedBracket], Maybe Int)
        go stack [] = (stack, Nothing)
        go stack ((letter, ix) : rest)
            | letter `elem` openingBrackets = go ((OpenedBracket letter ix) : stack) rest
            | letter `elem` closingBrackets =
                case stack of [] -> (stack, Just ix)
                              (OpenedBracket openingBracket _) : stackTail ->
                                  if doBracketsMatch openingBracket letter then go stackTail rest
                                  else (stack, Just ix)
            | otherwise = go stack rest
        (remainingStack, failedIndex) = go [] (zip string [1..])


main :: IO()
main = do
    string <- getLine
    case findIndexOfNonMatchingBracket string of Nothing -> print "Success"
                                                 Just ix -> print ix
