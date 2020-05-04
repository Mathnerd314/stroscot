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
-- Bn f g xn ... x1 = f (g xn...x1)
-- Cn f g xn ... x1 = (f xn ... x1) g
-- Wn f xn ... x1 = f xn...x1 xn...x1
-- In f xn ... x1 = f xn...x

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
  | V -- reference to top env variable (Zero), equivalent to N 1 Id
  | N Integer Repr -- N 1 e == e z, e closed, and similarly N n requires/uses n terms of context
  | W Integer Repr -- weakening, closed term that ignores n terms of context
  deriving Show

{-
z -> V -> N 1 (C Id)
s e -> W e -> let N l d = C (Kn 1) $$ e in N (l+1) d
lam
   V -> C Id
   C d -> C (Kn 1) $$ C d
   N 1 e -> e -- assert e is closed
   N l d -> N (l-1) d
   W e -> C (Kn 1) $$ e

  let ($$): type g a b. (g,a->b) repr -> (g,a) repr -> (g,b) repr =
   fun x y -> match (x,y) with
    | (C d1,N (d2,l)) -> N (S.(kBn l $? d1 $? d2),l)  (* D1 (D2 x) *)
    | (N (d1,l),C d2) -> N (S.(kCn l $? d1 $? d2),l)  (* (D1 x) D2 *)

    | (N (d1,l),N (d2,l2)) when l = l2 ->
        N (S.(kSn l $? d1 $? d2),l)  (* (D1 x) (D2 x) *)
    | (N (d1,l1),N (d2,l2)) when l1 < l2 ->
        let k = l2 - l1 in
        N (S.(kBn k $? (kSn l1 $? d1) $? d2),l2)
    | (N (d1,l1),N (d2,l2)) (* when l1 > l2 *) ->
        let k = l1 - l2 in
        let d1' = S.(kBn k $? kSn l2 $? d1) in
        N (S.(kCn k $? d1' $? d2), l1)

  let rec ($$): type g a b. (g,a->b) repr -> (g,a) repr -> (g,b) repr =
   fun e1 e2 ->
    match (e1,e2) with
    | (W e1, W e2)      -> W (e1 $$ e2)          (* both weakened *)
    | (W e, C d)        -> W (e $$ C d)
    | (C d, W e)        -> W (C d $$ e)
    | (W e,V)           -> N e                   (* e x *)
    | (V, W e)          -> N (C (S.(kC $! kI)) $$ e)
    (* (K (e1 xm ... x1)) (e2 xn ... x1 x0)
       == (e1 xm ... x1) ((e2 xn ... x1) x0)
       == B (e1 xm ... x1) (e2 xn ... x1) x0
    *)
    | (W e1, N e2)      -> N ((C S.kB) $$ e1 $$ e2)
    (* Mirror image *)
    | (N e1, W e2)      -> N ((C S.kC) $$ e1 $$ e2)

    | (C d, N e)        -> N ((C S.(kB $! d)) $$ e)
    (* (e1 x0) (e2 x0) == S e1 e2 x0 *)
    | (N e1, N e2)      -> N ((C S.kS) $$ e1 $$ e2)
    (* e x x *)
    | (N e, V)          -> N (C S.kS $$ e $$ C S.kI)
    (* x (e x) *)
    | (V, N e)          -> N (C S.(kS $! kI) $$ e)

    | (C d, V)          -> N (C d)                    (* d x *)
    | (V, C d)          -> N (C S.(kC $! kI $! d))    (* x d *)
    | (V,V)             -> N (C S.(kS $! kI $! kI))
    (* C C f g x --> C g f x --> g x f *)
    (* | (N e, C d)        -> N ((C S.kC) $$ e $$ C d)  below is better *)
    | (N e, C d)        -> N ((C S.(kC $! kC $! d)) $$ e)
-}

toRepr :: Deb -> Repr
toRepr (Hask h) = C (I h)
toRepr Zero = V
toRepr (Succ e) = case toRepr e of
  W n d -> W (n+1) d
  d -> W 1 d
toRepr (Lam l) = let lr = toRepr l in trace ("toRepr " ++ show l ++ " = " ++ show lr) $ case lr of
  V -> C Id
  C d -> C (Kn 1 ## d)
  N 1 e -> e -- eta contraction, assert e is closed
  N l e -> N (l-1) e
  W e -> (C $ Kn 1) $$ e
toRepr (App a b) = toRepr a $$ toRepr b

wToN :: Repr -> Repr
wToN (W k e) = let N l d = C (Kn k) $$ e in N (l+k) d

infixl 5 $$
x $$ y = let z = join x y in trace (show x ++ " _ " ++ show y ++ " = " ++ show z) z

join (C d1) (C d2) = C (d1 ## d2)
join (N l f) (N l2 g) = case compare l l2 of
   EQ -> N l $ C (Wn l) $$ (C (Bn l) $$ (C (Cn l) $$ f) $$ g)  -- (D1 x) (D2 x)
      -- (f xn ... x1) (g x2n ... xn+1)
      -- = (Cn f) (g x2n ... xn+1) xn ... x1
      -- = Bn (Cn f) g x2n x2n-1 ... x1
      -- = Wn (Bn (Cn f) g) xn ... x1
   LT -> let k = l2 - l in
      -- (f xn ... x1) (g xn+k ... x1)
      -- = Cn f (g xn+k ... x1) xn ... x1
      -- = Bn+k (Cn f) g xn+k ... xn+1 xn ... x1 xn ... x1
      -- = Wn (Bn+k (Cn f) g xn+k ... xn+1) xn ... x1
      -- = Bk Wn (Bn+k (Cn f) g) xn+k ... xn ... x1
      N l2 $ C (Bn k ## Wn l) $$ (C (Bn l2) $$ (C (Cn l) $$ f) $$ g)
   GT -> let k = l - l2 in
      -- (f xn+k ... x1) (g xn ... x1)
      -- = Bn (f xn+k ... x1) g xn ... x1
      -- = Cn+k (Bn f) g xn+k ... xn+1 xn ... x1 xn ... x1
      -- = Wn (Cn+k (Bn f) g xn+k ... xn+1) xn ... x1
      -- = Bk Wn (Cn+k (Bn f) g) xn+k ... xn ... x1
      N l2 $ C (Bn k ## Wn l2) $$ (C (Cn l) $$ (C (Bn l2) $$ f) $$ g)
join (C d) (N l e) = N l (C (Bn l) $$ C d $$ e)
join (N l d) (C e) = N l (C (Cn l) $$ d $$ C e)

join (W m e1) (W n e2) = W (max m n) $ e1 $$ e2 -- both weakened
join (W n e) (C d) = W n (e $$ C d)
join (C d) (W n e) = W n (C d $$ e)

--  (Kn k e1 xm ... x1) (e2 xn ... x1)
--  = (e1 xm ... xk+1) (e2 xn ... x1)
--  = (e1 xm ... xk+1) ((e2 xn ... xk+1) xk ... x1)
--  = Bk (e1 xm ... xk+1) (e2 xn ... xk+1) xk ... x1
--  = ... = (combinators) x_(max m n) ... x1
join (W k e1) (N n e2) = N (max k n) ((C $ Bn k) $$ e1 $$ e2)
join (N n e1) (W k e2) = N (max k n) ((C $ Cn k) $$ e1 $$ e2)
-- V element for optimizing I, section 4.1
join (W k e) V = N k e -- eta-reduce e x
join V (W k e) = N k (C (Cn k) $$ C Id $$ e)
join (N l e) V = N l e $$ N 1 (C Id) -- e x x
join V (N l e) = N 1 (C Id) $$ N l e -- x (e x) = (I x) (e x)
join (C d) V = N 1 (C d) -- d x eta reduces to d
join V (C d) = N 1 (C Id) $$ C d -- x d
join V V = N 1 (C (Wn 1 ## Id)) -- x x = WI

infixl 5 ~~
(~~) = App

-- page 16
paper_table_1  =
  [ lam $ lam z
  , lam . lam $ s
  , lam . lam $ s ~~ z
  , lam . lam $ z ~~ s
  , lam . lam . lam $ z ~~ x 2
  , lam . lam . lam $ (lam z) ~~ x 2
  , lam . lam . lam $ ((x 2) ~~ z) ~~ (s ~~ z)
  , lam . lam $ z ~~ s
  , lam . lam . lam $ z ~~ s ~~ x 2
  , lam . lam . lam . lam $ z ~~ s ~~ x 2 ~~ x 3
  ]
  where
      z = Zero
      s = Succ Zero
      x 0 = Zero
      x n = Succ (x (n-1))
      lam = Lam

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
wprime d f x = d (f x) x
theBcom f g x = f (g x)
theCcom f g x = f x g
theWcom f x = f x x

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
eval (Wn n) = h (unsafeCoerce nTimes (n-1) wprime theWcom)
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