import Data.List(nub,isPrefixOf)
import qualified Data.Set as Set

data M = Wk | Co
  deriving (Show,Eq,Ord)

-- derive all exponential implications up to a given length
data X = B [M] X | W [M] X | A
  deriving (Show,Eq,Ord)

addB x (B y b) = B (x:y) b
addB x b = B [x] b

ruleB a@(B (x:xs) g) b = Set.fromList [(a, addB x b)] `Set.union` Set.map (\(c,d) -> (addB x c, addB x d)) (ruleB (B xs g) b)
ruleB _ _ = Set.empty

ruleBd a b = Set.fromList [(addB Wk a, b),(addB Co a, b)]

addW x (W y b) = W (x:y) b
addW x b = W [x] b

ruleW a b@(W (x:xs) h) = Set.fromList [(addW x a, b)] `Set.union` Set.map (\(c,d) -> (addW x c, addW x d)) (ruleW a (W xs h))
ruleW _ _ = Set.empty

ruleWd a b = Set.fromList [(a, addW Wk b),(a, addW Co b)]

next (a,b) = Set.filter primitiveT $ ruleB a b `Set.union` ruleBd a b `Set.union` ruleW a b `Set.union` ruleWd a b

primitive (B (ma:mb:xs) x) | ma == mb = False
primitive (W (ma:mb:xs) x) | ma == mb = False
primitive (B (ma:mb:mc:xs) x) = False
primitive (W (ma:mb:mc:xs) x) = False
primitive (B ma (W mb (B mc (W md x)))) | mc `isPrefixOf` ma && mb `isPrefixOf` md = False
primitive (W ma (B mb (W mc (B md x)))) | mc `isPrefixOf` ma && mb `isPrefixOf` md = False
primitive _ = True

match A A A A = True
match A A (B ma c) (B mb d) | ma == mb = match A A c d
match A A (W ma c) (W mb d) | ma == mb = match A A c d
match (B ma a) b (B mb c) d | ma == mb = match a b c d
match (W ma a) b (W mb c) d | ma == mb = match a b c d
match a (B ma b) c (B mb d) | ma == mb = match a b c d
match a (W ma b) c (W mb d) | ma == mb = match a b c d
match _ _ _ _ = False

patterns = [(A,A)]

primitiveT (a,b) = primitive a && primitive b -- && not (any (\(c,d) -> (a,b) /= (c,d) && match c d a b) patterns)

things = iterate (\s -> Set.unions $ Set.map next s) (Set.singleton (A,A))

printM Wk = 'w'
printM Co = 'c'

printX A = "A"
printX (B m x) = "!" ++ map printM m ++ printX x
printX (W m x) = "?" ++ map printM m ++ printX x

printT (a,b) = printX a ++ " |- " ++ printX b

p :: Foldable t => t (X, X) -> IO ()
p = mapM_ (putStrLn . printT)

findEquiv ls = Set.filter (\(a,b) -> a /= b && (b,a) `Set.member` ls) ls

trans set (x,y) = any (\(a,b) -> x == a && any (\(c,d) -> b == c && d == y) set) set

elimTrans set = let nor = filter (\(a,b) -> a /=b) set in filter (\e -> not (trans nor e)) nor
