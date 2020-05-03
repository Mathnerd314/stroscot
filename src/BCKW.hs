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


--    | N: S.u S.repr * int -> ('g,'a) repr

    -- | (C d1,N (d2,l)) -> N (Bn l ## d1 ## d2) l  -- D1 (D2 x)
    -- | (N (d1,l),C d2) -> N (Cn l ## d1 ## d2) l  -- (D1 x) D2
    -- | (N (d1,l),N (d2,l2)) when l == l2 ->
    --     N (Sn l ## d1 ## d2) l  -- (D1 x) (D2 x)
    -- | (N (d1,l1),N (d2,l2)) | l1 < l2 ->
    --     let k = l2 - l1 in
    --     N (S.(kBn k ## (kSn l1 ## d1) ## d2),l2)
    -- | (N (d1,l1),N (d2,l2)) | l1 > l2  ->
    --     let k = l1 - l2 in
    --     let d1' = S.(kBn k ## kSn l2 ## d1) in
    --     N (S.(kCn k ## d1' ## d2), l1)

--   let z = N (Id, 1)
--   let s fun e ->
--     let N (d,l) = (C S.kK) $$ e in N (d,l+1)
--  let lam: type a g b. (a*g,b) repr -> (g,a->b) repr = function
--     | N (d,1)    -> C d
--     | N (d,l)    -> N (d,l-1)

-- ski :: Deb -> Com
-- ski deb = case deb of
--   Zero                           -> I
--   Succ d    | x@(n, _) <- ski d  -> f (0, K) x
--   App d1 d2 | x@(a, _) <- ski d1
--             , y@(b, _) <- ski d2 -> f x y
--   Lam d -> case sharing d of
--       [] -> K :# e
--       _ -> e
--   where
--   f x y = case (sharing x, sharing y) of
--     (0, 0)             ->         x :# y
--     (0, n)             -> Bn n :# x :# y
--     (n, 0)             -> Cn n :# x :# y
--     (n, m) | n == m    -> Wn n :# x :# y
--            | n < m     ->                Bn (m - n) :# (Wn n :# x) :# y
--            | otherwise -> Cn (n - m) :# (Bn (n - m) :#  Wn m :# x) :# y


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

-- NB: the choice of ordering (x2 x vs x x2) is what fixes the evaluation order
-- but we always do LTR for simplicity

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

{-
source :: Parsec String [String] Deb
source = term where
  term = lam <|> app
  lam = do
    orig <- getState
    vs <- between lam0 lam1 (many1 v)
    modifyState (reverse vs ++)
    t <- term
    putState orig
    pure $ iterate Lam t !! length vs
    where lam0 = str "\\" <|> str "\0955"
          lam1 = str "->" <|> str "."
  v   = many1 alphaNum <* ws
  app = foldl1' App <$> many1 ((ind =<< v) <|>
    between (str "(") (str ")") term)
  ind s = (iterate Succ Zero !!) .
    maybe (error $ s ++ " is free") id . elemIndex s <$> getState
  str = (>> ws) . string
  ws = many (oneOf " \t") >> optional (try $ string "--" >> many (noneOf "\n"))

amain = prInteger $ logBulk $ Sn 1234
main = do
  let
    s = "\\l.l(\\h t x.t(\\c n.c h(x c n)))(\\a.a)(\\a b.b)"
    Right out = logBulk <$> snd . ski <$> runParser source [] "" s
  prInteger out
-}

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