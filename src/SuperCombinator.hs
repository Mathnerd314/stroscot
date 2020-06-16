import Control.Applicative hiding ( Const )
import Control.Monad
import System.IO
import System.Directory
import System.Environment
import Control.Monad.State

import Debug.Trace
import Data.Char

M = D (F I)
D x = W I
F z = z y
I x = x

W I (F I)

P = F3 G3
G3 y = D3 (y A)
D3 = W (W I)
$I x = x I
F3 = W (W (W ($I)))

P
F3 G3
W (W (W ($I))) G3
W (W (($I) G3)) G3
W (W ($I))) G3 G3
W ($I) G3 G3 G3
($I) G3 G3 G3 G3
G3 I G3 G3 G3

W (W (W ($I))) \y. (D3 (y A))
W (W (W ($I))) \y. (W (W I) (y A))

W f Z
f (\y. Z y) (\y. Z y)
W (f \y.) (Z y)

data Name = UN String  -- user name
          | MN String Int -- machine generated name
  deriving Eq

instance Show Name where
    show (UN str) = "_U_"++str
    show (MN str i) = "_M_"++show i++"_"++str

showuser (UN str) = str
showuser (MN str i) = "["++str++"_"++show i++"]"

quotename [] = ""
quotename ('_':cs) = "__"++quotename cs
quotename ('\'':cs) = "_PR_"++quotename cs
quotename ('?':cs) = "_QU_"++quotename cs
quotename ('$':cs) = "_DO_"++quotename cs
quotename ('#':cs) = "_HA_"++quotename cs
quotename ('@':cs) = "_AT_"++quotename cs
quotename (c:cs) | isAlphaNum c = c:(quotename cs)
                 | otherwise = "_" ++ show (fromEnum c) ++ "_" ++ quotename cs

showC n = quotename (show n)

type Type = Integer -- untyped lang, so Type = arity
type Context = [(Name,Type)] -- Name, arity

-- | Get the arity of a definition in the context
arity :: Name -> Context -> Integer
arity x ctxt = case lookup x ctxt of
                  Nothing -> error $ "No such function " ++ show x
                  Just args -> args

type Tag = Int
type HFun = Expr -> Expr
data Expr = V Int -- Locally bound name
          | R Name -- Global reference
          | App Expr [Expr] -- Function application
          | Lam Name Type Expr -- inner lambda
          | Error String -- Exit with error message
          | Impossible -- Claimed impossible to reach code
--           | WithMem Allocator Expr Expr -- evaluate with manual allocation
--           | ForeignCall Type String [(Expr, Type)] -- Foreign function call
--           | LazyForeignCall Type String [(Expr, Type)] -- Foreign function call
  deriving Eq

instance Show Expr where
    show (V i) = "var" ++ show i
    show (R n) = show n
    show (App f as) = show f ++ show as
    show (Lazy e) = "%lazy(" ++ show e ++ ")"
    show (Let n t v e) = "let " ++ show n ++ ":" ++ show t ++ " = " ++
                         show v ++ " in " ++ show e
    show (Lam n t e) = "\\ " ++ show n ++ ":" ++ show t ++ " . " ++
                         show e
    show (Error e) = "error(" ++ show e ++ ")"
    show Impossible = "Impossible"

-- | Supercombinator definitions
data Func = Bind { fun_args :: [(Name, Type)],
                   locals :: Int, -- total number of locals
                   defn :: Expr,
                   flags :: [CGFlag]}
  deriving Show

class SubstV x where
    subst :: Int -> Expr -> x -> x

instance SubstV a => SubstV [a] where
    subst v rep xs = map (subst v rep) xs

instance SubstV (Expr, Type) where
    subst v rep (x, t) = (subst v rep x, t)

instance SubstV Expr where
    subst v rep (V x) | v == x = rep
    subst v rep (App x xs) = App (subst v rep x) (subst v rep xs)
    subst v rep (Lam n t e) = Lam n t (subst v rep e)
    subst v rep x = x

data CGFlag = Inline | Strict
  deriving (Show, Eq)

data Result r = Success r
              | Failure String String Int
    deriving (Show, Eq)

instance Functor Result where
  fmap = liftM

instance Applicative Result where
  pure  = return
  (<*>) = ap

instance Monad Result where
    (Success r)   >>= k = k r
    (Failure err fn line) >>= k = Failure err fn line
    return              = Success
    fail s              = Failure s "(no file)" 0

instance Alternative Result where
  (<|>) = mplus
  empty = mzero

instance MonadPlus Result where
    mzero = Failure "Error" "(no file)" 0
    mplus (Success x) _ = (Success x)
    mplus (Failure _ _ _) y = y

appForm :: Expr -> Bool
appForm (App _ _) = True
appForm (V _) = True
appForm (R _) = True
appForm _ = False


checkAll :: Monad m => [CompileOptions] -> [Decl] -> m (Context, [Decl])
checkAll opts xs = do let ctxt = mkContext xs
                      ds <- ca (mkContext xs) xs
                      return (mkContext ds,ds)
   where ca ctxt [] = return []
         ca ctxt ((Decl nm rt fn exp fl):xs) =
             do (fn', newds) <- scopecheck (checkLevel opts) ctxt nm fn
                xs' <- ca ctxt (newds ++ xs)
                return $ (Decl nm rt fn' exp fl):xs'
         ca ctxt (x:xs) =
             do xs' <- ca ctxt xs
                return (x:xs')

         mkContext [] = []
         mkContext ((Decl nm rt (Bind args _ _ _) _ _):xs) =
             (nm,(map snd args, rt)):(mkContext xs)
         mkContext ((Extern nm rt args):xs) =
             (nm,(args, rt)):(mkContext xs)
         mkContext (_:xs) = mkContext xs

-- Check all names are in scope in a function, and convert global
-- references (R) to local names (V). Also, if any lazy expressions are
-- not already applications, lift them out and make a new
-- function. Returns the modified function, and a list of new
-- declarations. The new declarations will *not* have been scopechecked.

-- Do Lambda Lifting here too

scopecheck :: Monad m => Int -> Context -> Name -> Func -> m (Func, [Decl])
scopecheck checking ctxt nm (Bind args locs exp fl) = do
       (exp', (locs', _, ds)) <- runStateT (tc (v_ise args 0) exp) (length args, 0, [])
       return $ (Bind args locs' exp' fl, ds)
 where
   getRoot (UN nm) = nm
   getRoot (MN nm i) = "_" ++ nm ++ "_" ++ show i
   tc env (R n) = case lookup n env of
                     Nothing -> case lookup n ctxt of
                        Nothing -> if (checking > 0) then lift $ fail $ "Unknown name " ++ showuser n
                                   else return $ Const (MkInt 1234567890)
                        (Just _) -> return $ R n
                     (Just i) -> return $ V i
   tc env (App f as) = do
               f' <- tc env f
               as' <- mapM (tc env) as
               return $ App f' as'

-- Make a new function, with current env as arguments, and add as a decl
   tc env (Lam n ty e) = lift e [(n,ty)] where
       lift (Lam n ty e) args = lift e ((n,ty):args)
       lift e args = do (maxlen, nextn, decls) <- get
                        let newname = MN (getRoot nm) nextn
                        let newargs = zip (map fst env) (repeat TyAny)
                                         ++ reverse args
                        let newfn = Bind newargs 0 e []
                        let newd = Decl newname TyAny newfn Nothing []
                        put (maxlen, nextn+1, newd:decls)
                        return $ App (R newname) (map V (map snd env))
   tc env x = return x

-- Turn the argument list into a mapping from names to argument position
-- If any names appear more than once, use the last one.
-- We're being very tolerant of input here...
v_ise [] _ = []
v_ise ((n,ty):args) i = let rest = v_ise args (i+1) in
                            case lookup n rest of
                              Nothing -> (n,i):rest
                              Just i' -> (n,i'):rest
      --  where dropArg n [] = []
      --        dropArg n ((x,i):xs) | x == n = dropArg n xs
      --                             | otherwise = (x,i):(dropArg n xs)
-- This is scope checking without the lambda lifting. Of course, it would be
-- better to separate the two anyway... FIXME later...

class RtoV a where
    rtov :: [(Name, Int)] -> a -> a
    doRtoV :: a -> a
    doRtoV = rtov []

instance RtoV a => RtoV [a] where
    rtov env xs = map (rtov env) xs

instance RtoV a => RtoV (a, Type) where
    rtov env (x, t) = (rtov env x, t)

instance RtoV Func where
    rtov env (Bind args locs def flags)
         = Bind args locs (rtov (v_ise args 0) def) flags

instance RtoV Expr where
    rtov v (R x) = case lookup x v of
                     Just i -> V i
                     _ -> R x
    rtov v (App f xs) = App (rtov v f) (rtov v xs)
    rtov v (Lam n ty sc) = Lam n ty (rtov ((n,length v):v) sc)
    rtov v x = x
