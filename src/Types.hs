---------------------------------------------------------------------
-- CALL STACK

-- Invariant: Every key must have its Id in the set
data Stack = Stack {
  topStack :: (Maybe Key)
  ks :: [Either Key [String]]
  is :: !(Set.HashSet Id)
 } deriving Show

emptyStack :: Stack
emptyStack = Stack Nothing [] Set.empty

exceptionStack :: Stack -> SomeException -> ShakeException
exceptionStack stack@(Stack _ xs1 _) (callStackFromException -> (xs2, e)) =
    ShakeException
        (showTopStack stack)
        (xs ++ ["* Raised the exception:" | not $ null xs])
        e
    where
        xs = concatMap f $ reverse xs1 ++ [Right xs2]
        f (Left x) = ["* Depends on: " ++ show x]
        f (Right x) = map ("  at " ++) x


showTopStack :: Stack -> String
showTopStack = maybe "<unknown>" show . topStack

addCallStack :: [String] -> Stack -> Stack
-- use group/head to squash adjacent duplicates, e.g. a want does an action and a need, both of which get the same location
addCallStack xs (Stack t a b) = Stack t (Right xs : dropWhile (== Right xs) a) b

addStack :: Id -> Key -> Stack -> Either SomeException Stack
addStack i k (Stack _ ks is)
    | i `Set.member` is = Left $ toException $ exceptionStack stack2 $ errorRuleRecursion (typeKey k) (show k)
    | otherwise = Right stack2
    where stack2 = Stack (Just k) (Left k:ks) (Set.insert i is)

