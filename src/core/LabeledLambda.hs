import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State.Lazy
import Data.Char (toUpper)

data Label = Atom String | Concat [Label] | Underline Label | Overline Label
 deriving (Show, Eq, Ord)

printLabel :: Label -> String
printLabel (Atom s) = s
printLabel (Concat ls) = concat (map printLabel ls)
printLabel (Underline l) = "\\underline{" ++ printLabel l ++ "}"
printLabel (Overline l) = "\\overline{" ++ printLabel l ++ "}"

concatL :: Label -> Label -> Label
concatL (Concat a) (Concat b) = case (a ++ b) of
  [a] -> a
  xs -> Concat xs
concatL (Concat a) b = concatL (Concat a) (Concat [b])
concatL a (Concat b) = concatL (Concat [a]) (Concat b)
concatL a b = Concat [a,b]

data LLam = App Label LLam LLam | Lam Label String LLam | Var Label String
 deriving (Show, Eq, Ord)

printLLam :: LLam -> String
printLLam (Var l s) =  s ++ "^{" ++ printLabel l ++ "}"
printLLam (Lam l x m) =  "\\left(\\lambda " ++ x ++ "." ++ printLLam m ++ "\\right)^{" ++ printLabel l ++ "}"
printLLam (App l m n) =  "\\left(" ++ printLLam m ++ " " ++ printLLam n ++ "\\right)^{" ++ printLabel l ++ "}"

addLabel :: Label -> LLam -> LLam
addLabel a (Var l s) = Var (concatL a l) s
addLabel a (App l m n) = App (concatL a l) m n
addLabel a (Lam l x m) = Lam (concatL a l) x m

freevars :: LLam -> Set String
freevars (Var l v) = Set.singleton v
freevars (Lam l v b) = Set.delete v (freevars b)
freevars (App l a b) = freevars a `Set.union` freevars b

freeIn x = Set.member x . freevars

mkNew :: State Int String
mkNew = do
  n <- get
  put (n+1)
  return $ "x" ++ show n


-- sub x n m is M[N/x]
sub :: String -> LLam -> LLam -> State Int LLam
sub x s t@(Var l y)
    | x == y        = return $ addLabel l s
    | otherwise     = return $ t
sub x s (App l t1 t2) = do
  t1' <- sub x s t1
  t2' <- sub x s t2
  return $ App l t1' t2'
sub x s t@(Lam l y t1)
    | not $ freeIn x t  = return $ t
    | freeIn y s        = do
      z <- mkNew
      h <- sub y (Var (Concat []) z) t1
      k <- sub x s h
      return $ Lam l z k
    | otherwise         = do
      k <- sub x s t1
      return $ Lam l y k

betareduce :: LLam -> State Int LLam
betareduce (App bl (Lam al x m) n) = do
  t <- sub x (addLabel (Underline al) n) m
  return $ addLabel (Concat [bl, Overline al]) t

degree :: LLam -> Label
degree (App bl (Lam al x m) n) = al

isRedex :: LLam -> Bool
isRedex (App bl (Lam al x m) n) = True
isRedex _ = False

familyReduce :: Label -> LLam -> State Int LLam
familyReduce l t | isRedex t && degree t == l = familyReduce l =<< betareduce t
familyReduce l (App l2 m n) = App l2 <$> familyReduce l m <*> familyReduce l n
familyReduce l (Lam l2 x m) = Lam l2 x <$> familyReduce l m
familyReduce l (Var l2 x) = pure $ Var l2 x

mkL :: State Int Label
mkL = do
  n <- get
  put (n+1)
  return $ Atom [map toUpper "abcdefghijklmnopqrstuvwxyz" !! n]

lam x y = pure Lam <*> mkL <*> pure x <*> y
app x y = pure App <*> mkL <*> x <*> y
var x = pure Var <*> mkL <*> pure x



-- delta (\h. delta (h I))
delta v = lam v (app (var v) (var v))
mx = app (delta "x")
    (lam "h" (app (delta "y")
      (app (var "h") (lam "i" (var "i")))))
m = fst $ runState mx 0

doReduce :: [Label] -> LLam -> Int -> [LLam]
doReduce (l:ls) m i = do
  let (m', i') = runState (familyReduce l m) i
  m' : doReduce ls m' i'
doReduce [] m i = []

ms :: [LLam]
ms = m : doReduce
            [ Atom "B"
            , Atom "H"
            , Concat [Atom "D", Underline (Atom "B"), Atom "F"]
            , Concat [Atom "M",Underline (Concat [Atom "D",Underline (Atom "B"),Atom "F"]),Atom "E",Underline (Atom "B"),Atom "F"]
            , Concat [Atom "M",Underline (Concat [Atom "M",Underline (Concat [Atom "D",Underline (Atom "B"),Atom "F"]),Atom "E",Underline (Atom "B"),Atom "F"]),Atom "N"]
            ] m 0

main :: IO ()
main = do
  forM_ ms $ \mx ->
    putStrLn $ printLLam mx
