{-# LANGUAGE KindSignatures #-}
import Data.List
import Text.Parsec
import GHC.Exts
import Unsafe.Coerce
import System.IO.Unsafe
import Debug.Trace
import Control.Arrow(second)

newtype Hask = H Any
instance Show Hask where
  show _ = "^"

h = H . unsafeCoerce

data Deb = Hask Hask | Zero | Succ Deb | Lam Deb | App Deb Deb deriving Show

infixl 5 :#
infixl 5 ##
data Com = Com :# Com | Id | I Hask | Kn Integer | Wn Integer | Bn Integer | Cn Integer

trivial :: Com -> Bool
trivial Id = True
trivial (Wn 0) = True
trivial (Bn 0) = True
trivial (Cn 0) = True
trivial (Kn 0) = True
trivial _ = False

x ## y | trivial x = y
x ## y = x :# y

data Sharing = Unused | Linear | OnLeft | OnRight | Shared Integer deriving (Show, Eq, Ord)

-- An intermediate representation for lambda expressions
-- Generally open code with variables x  y ... z where
-- z has the highest index and x has index 0
-- is represented as
-- D z y ... x where D is closed
-- If a term does not have a variable with index 0 as
-- free, we add it, using combinators
-- Variables are eliminated by eta-reduction
data Repr
  = C Com -- closed code
  | V -- reference to top env variable (Zero)
  | N Repr -- N e === (s e) z
  | W Repr -- Weakened K e
  deriving Show

toRepr :: Deb -> Repr
toRepr (Hask h) = C (I h)
toRepr Zero = V
toRepr (Succ e) = W (toRepr e)
toRepr (Lam l) = case toRepr l of
  V -> C Id
  C d -> C (Kn 1 ## d)
  N e -> e -- eta contraction
  W e -> join (C $ Kn 1) e
toRepr (App a b) = toRepr a $$ toRepr b

infixl 5 $$
($$) = join
join (W e1) (W e2) = W $ e1 $$ e2 -- both weakened
join (W e) (C d) = W (e $$ C d)
join (C d) (W e) = W (C d $$ e)
join (W e) V = N e -- e x
join V (W e) = N (C (Cn 1 ## Id) $$ e)
--  (K (e1 xm ... x1) (e2 xn ... x1 x0)
--  == (e1 xm ... x1) ((e2 xn ... x1) x0)
--  == B (e1 xm ... x1) (e2 xn ... x1) x0
join (W e1) (N e2) = N ((C $ Bn 1) $$ e1 $$ e2)
    -- Mirror image
join (N e1) (W e2) = N ((C $ Cn 1) $$ e1 $$ e2)
-- (e1 x0) (e2 x0)
-- N ((C $ Sn 1) $$ e1 $$ e2)
    -- == W (\x1 x2. e1 x1 (e2 x2)) x0
    -- == W (\x1. B (e1 x1) e2)
    -- == W (B (C e1) e2)
-- NB: the choice of ordering (x2 x vs x x2) is what fixes the evaluation order
-- but we always do this order for simplicity
join (N e1) (N e2) = N (C (Wn 1) $$ (C (Bn 1) $$ (C (Cn 1) $$ e1) $$ e2))
join (N e) V = N (C (Wn 1) $$ e) -- e x x
join V (N e) = join (N (C Id)) (N e) -- x (e x) = (I x) (e x)
join (C d) (N e) = N ((C (Bn 1 ## d)) $$ e)
join (C d) V = N (C d) -- d x
join V (C d) = N (C (Cn 1 ## Id ## d)) -- x d
join V V = N (C (Wn 1 ## Id))
    -- C C f g x - = C g f x - = g x f
    -- (N e) (C d) = N ((C $ Cn 1) $$ e $$ C d)  below is better
join (N e) (C d) = N ((C (Cn 1 ## Cn 1 ## d)) $$ e)
join (C d1) (C d2) = C (d1 ## d2)

infixl 5 ~~
(~~) = App

tests =
  [ lam z
  , lam $ lam z
  , lam . lam $ s
  , lam . lam $ s ~~ z
  , lam . lam $ z ~~ s
  , lam . lam . lam $ z ~~ x 2
  , lam . lam . lam $ (lam z) ~~ x 2
  , lam . lam . lam $ ((x 2) ~~ z) ~~ (s ~~ z)
  , lam . lam . lam $ z ~~ s ~~ x 2
  , lam (lam (lam (lam (z ~~ s ~~ x 2 ~~ x 3))))
  , lam . lam . lam . lam $ ((x 0 ~~ x 2) ~~ (x 1 ~~ x 3))
  , lam . lam . lam . lam . lam . lam . lam . lam $
      ((x 0 ~~ x 4) ~~ (x 2 ~~ x 6)) ~~ ((x 1 ~~ x 5) ~~ (x 3 ~~ x 7))
  ]
  where
      z = Zero
      s = Succ Zero
      x 0 = Zero
      x n = Succ (x (n-1))
      lam = Lam


nshow 1 = ""
nshow n = show n

instance Show Com where
  show (Wn n) = "W" ++ nshow n
  show (I _) = "~"
  show Id = "I"
  show (Kn n) = "K" ++ nshow n
  show (l :# r) = (show l ++) $ case r of
    _ :# _ -> "(" ++ show r ++ ")"
    _      ->        show r
  show (Bn n) = "B" ++ nshow n
  show (Cn n) = "C" ++ nshow n

-- from GHC Utils
-- | Compose a function with itself n times.  (nth rather than twice)
nTimes :: Integer -> (a -> a) -> (a -> a)
nTimes 0 _ = id
nTimes 1 f = f
nTimes n f = f . nTimes (n-1) f

bprime d f g x = d f (g x)
cprime d f g x = d (f x) g
theBcom f g x = f (g x)
theCcom f g x = f x g

arity Id = 0
arity (I _) = 0
arity (Kn n) = n + 1
arity (Wn n) = n + 1
arity (Bn n) = n + 2
arity (Cn n) = n + 2
arity (a :# b) = max 0 (arity a - 1)

eval :: Com -> Hask
eval (I x) = x
eval Id = h id
-- eval (x :# y) | trivial x = y

-- eval (Bn n :# f :# g :# x) = Bn (n-1) ## f :# (g :# x)
-- eval (Cn n :# f :# g :# x) = Cn (n-1) ## (f :# x) :# g

-- eval (Bn n :# f) | trivial f = Id

-- -- eval (Bn n :# f :# g) | trivial g = Bn (n-1) ## f :# (g :# x)
-- -- eval (Cn n :# f :# g) | trivial g = Cn (n-1) ## (f :# x) :# g

-- eval (Kn n :# x :# y) = Kn (n-1) ## x
-- eval (Wn n :# x :# y) = Wn (n-1) ## (x :# y) :# y


eval (Kn n) = h (unsafeCoerce nTimes n const)
eval (Wn n) = h (\f a -> unsafeCoerce nTimes n ($ a) (f a) )
eval (Bn n) = h (unsafeCoerce nTimes (n-1) bprime theBcom)
eval (Cn n) = h (unsafeCoerce nTimes (n-1) cprime theCcom)
eval (x :# y) = x @@ y

infixl 5 @@

(@@) = applyh

-- for testing value-level correctness of the reduction; applyh should equal apply
applyh x y | H a <- eval x, H b <- eval y = h (unsafeCoerce a b)

apply :: Com -> Com -> Hask
apply Id x = eval x
apply x y | trivial x = eval y
apply (x :# y) z = reduce x y z
apply (Wn n) a = undefined

reduce :: Com -> Com -> Com -> Hask
reduce = undefined


-----------
{-
-- | Do one step of normal order reduction and return the result.
step :: Deb -> Maybe Deb
step (App (Lam e) r) = Just (sub 0 r e)  -- found a redex, do beta reduction / substitution
-- otherwise search for redex
step (App l r) = case step l of
  Just l' -> Just (App l' r)
  Nothing -> fmap (App l) (step r)
step (Abs e) = fmap Abs (step e)
step (Succ e) = fmap Succ (step e)
step _ = Nothing

-- | Evaluate an expression to normal form using normal order evaluation.
--   Note that this function will not terminate if the reduction never
--   reaches a normal form!
eval :: Deb -> Deb
eval e = case step e of
           Nothing -> e
           Just e' -> eval e'

-- | Variable substitution. `sub v e1 e2` substitutes e1 for every v in e2.
--
--   Each time we enter a new abstraction in e2, we must:
--    1. Increment the v that we're looking for.
--    2. Increment all of the free variables in e1 since now the references
--       will have to skip over one more lambda to get to the lambda they
--       refer to.
--
--   For each variable reference v' in e2, there are three possibilities:
--     1. It's the variable v we're looking for, in which case we replace it.
--     2. It's a variable bound in e2 (v' < v), in which case we do nothing.
--     3. It's a variable that is free in e2 (v' > v), in which case we
--        decrement it since now the reference has to skip over one less
--        lambda (i.e. the lambda that we beta-reduced away) to get to the
--        lambda it refers to.
--
--   >>> sub 0 (Succ Zero) (Lam (Succ Zero))
--   Lam (Succ $ Succ Zero)
--
sub :: Integer -> Deb -> Deb -> Deb
sub v e (App l r) = App (sub v e l) (sub v e r)
sub v e (Lam e')  = Lam (sub (v+1) e e') -- increment v
sub 0 e (Succ e')  = sub (-1) e e' -- not v and free in e2, decrement it to skip over the lambda we're disappearing
sub v e (Succ e')  = Succ (sub (v-1) e e')
sub 0 e Zero = e -- found a v, replace it!
sub _ e Zero = Zero -- not v and bound in e2, leave it alone

-- | Increment the free variables in an expression.
--
--   The argument d (for "depth") indicates the number of abstractions we
--   have recursed into so far. A variable that is smaller than the depth
--   is not free, and so should not be incremented.
--
--   >>> inc 0 (Ref 0)
--   Ref 1
--
--   >>> inc 0 (App (Ref 1) (Abs (Ref 0)))
--   App (Ref 2) (Abs (Ref 0))
--
inc :: Int -> Exp -> Exp
inc d (App l r) = App (inc d l) (inc d r)
inc d (Abs e)   = Abs (inc (d+1) e)
inc d (Ref v)   = Ref (if v < d then v else v+1)
-}