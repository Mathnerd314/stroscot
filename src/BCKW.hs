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

-- variables, and where they are
-- xs !! 0 / head xs = location of Zero
-- these are director strings a la Sinot https://link.springer.com/chapter/10.1007%2F3-540-44881-0_5
-- (with a down arrow (Linear), because I like it)
sharing :: Deb -> [Sharing]
sharing (Hask x) = []
sharing Zero = [Linear]
sharing (Succ x) = Unused : sharing x
sharing (Lam d) = case sharing d of
  [] -> []
  x:xs -> xs
sharing (App a b) =
  join (map (replace OnLeft) $ sharing a)
      (map (replace OnRight) $ sharing b) where

  replace y Unused = Unused
  replace y (Shared s) = Shared s
  replace y _ = y

  join [] xs = xs
  join xs [] = xs
  join (x:xs) (y:ys) = (: join xs ys) $ case (x,y) of
    (Unused,y) -> y
    (x,Unused) -> x
    (OnLeft,OnRight) -> Shared 1
    (Shared n,OnRight) -> Shared (n+1)
    (OnLeft, Shared n) -> Shared (n+1)
    (Shared m, Shared n) -> Shared (m+n)

  -- Generally open code with variables x  y ... z where
  -- z has the highest index and x has index 0
  -- is represented as
  -- D z y ... x where D is closed
  -- If a term does not have a variable with index 0 as
  -- free, we add it, using combinators
  -- Variables are eliminated by eta-reduction



-- module LinearConv(S:BulkSKI) (* : Lam *) = struct
--   type ('g,'a) repr =
--     | C: 'a S.repr -> ('g,'a) repr   (* closed code *)
--     | N: S.u S.repr * int -> ('g,'a) repr

--   let ($$): type g a b. (g,a->b) repr -> (g,a) repr -> (g,b) repr =
--    fun x y -> match (x,y) with
--     | (C d1,C d2)     -> C (S.(d1 $! d2))
--     | (C d1,N (d2,l)) -> N (S.(kBn l $? uclose d1 $? d2),l)  (* D1 (D2 x) *)
--     | (N (d1,l),C d2) -> N (S.(kCn l $? d1 $? uclose d2),l)  (* (D1 x) D2 *)
--     | (N (d1,l),N (d2,l2)) when l = l2 ->
--         N (S.(kSn l $? d1 $? d2),l)  (* (D1 x) (D2 x) *)
--     | (N (d1,l1),N (d2,l2)) when l1 < l2 ->
--         let k = l2 - l1 in
--         N (S.(kBn k $? (kSn l1 $? d1) $? d2),l2)
--     | (N (d1,l1),N (d2,l2)) (* when l1 > l2 *) ->
--         let k = l1 - l2 in
--         let d1' = S.(kBn k $? kSn l2 $? d1) in
--         N (S.(kCn k $? d1' $? d2), l1)

--   let uI = S.(uclose kI)                (* for the sake of polymorphism in z *)
--   let z:   ('a*'g,'a) repr = N (uI, 1)
--   let s:   ('b*'g,'a) repr -> (_*('b*'g),'a) repr = fun e ->
--     let N (d,l) = (C S.kK) $$ e in N (d,l+1)

--   let lam: type a g b. (a*g,b) repr -> (g,a->b) repr = function
--     | C k        -> C (S.(kK $! k))
--     | N (d,1)    -> C (S.uopen d)
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

-- | This is a translation from lambdas to combinators,
--   similar to Oleg's http://okmij.org/ftp/tagless-final/ski.pdf
--   but using the W combinator instead of S, W = SI
--   AFAICT it's linear in the same restricted way Oleg's is linear, but who knows.
--   NB: Oleg's lambdas use CVNW for the constructors, which is crazy.
bckw :: Deb -> Com
bckw (Hask x) = I x
bckw Zero = Id
bckw (Succ x) = bckw x
bckw (Lam d) = case sharing d of
  [] -> Kn 1 ## bckw d
  Unused : _ -> Kn 1 ## bckw d
  -- eta contraction
  _ : _ -> bckw d
bckw (App xx yy) = distributeVarsT (sharing xx) (sharing yy) xx yy where

t = Lam . Lam $ App (Succ Zero) (App (Succ Zero) Zero)
-- should convert to WB

-- B x y can be reduced to x y if it's the root. Hence this version for the inner
distributeVarsT x y l r = trace ("\n" ++ show x ++ "/" ++ show y ++ ": " ++ show l ++ " _ " ++ show r) $ distributeVars x y l r

distributeVars :: [Sharing] -> [Sharing] -> Deb -> Deb -> Com
-- f g, closed term application
distributeVars [] [] x y = bckw x ## bckw y
-- lazy weakening
distributeVars (Unused:a) [] x y = distributeVarsT a [] x y
distributeVars [] (Unused:b) x y = distributeVarsT [] b x y
distributeVars (Unused:a) (Unused:b) x y = distributeVars a b x y
-- eta-optimization for small terms
-- \x. x x
distributeVars [Linear] [Linear] x y = Wn 1 ## distributeVarsT [] [] x y
-- \y. f y -> f
distributeVars (Unused:_) [Linear] x y = bckw x
distributeVars [] [Linear] x y = bckw x
-- \x. x f
distributeVars [Linear] (Unused:_) x y = distributeVars Cn 1 ## bckw x ## bckw y
distributeVars [Linear] [] x y = Cn 1 ## bckw x ## bckw y
-- (K (e1 xm ... x1) (e2 xn ... x1 x0)
--    == (e1 xm ... x1) ((e2 xn ... x1) x0)
--    == B (e1 xm ... x1) (e2 xn ... x1) x0
distributeVars [] (_:_) x y = Bn 1 ## bckw x ## bckw y
distributeVars (Unused:_) (_:_) x y = Bn 1 ## bckw x ## bckw y
distributeVars (_:_) (Unused:_) x y = Cn 1 ## bckw x ## bckw y
-- C C f g x --> C g f x --> g x f
-- which is better than
-- distributeVars (_:_) [] x y = Cn 1 ## bckw x ## bckw y
-- because g is closed and f may disappear
distributeVars (_:_) [] f g = (Cn 1 ## Cn 1 ## bckw g) ## bckw f

-- f x (g x) = S f g x = W (\x y. f x (g y)) x
distributeVars (_:_) (_:_) x y = Wn 1 ## bckw (Lam (Lam (App (Succ x) y)))

    -- (* (e1 x0) (e2 x0) == S e1 e2 x0 *)
    -- | (N e1, N e2)      -> N ((C S.kS) $$ e1 $$ e2)
    -- (* x (e x) *)
    -- | (V, N e)          -> N (C S.(kS $! kI) $$ e)

infixl 5 $$
($$) = App

tests =
  [ lam z
  , lam $ lam z
  , lam . lam $ s
  , lam . lam $ s $$ z
  , lam . lam $ z $$ s
  , lam . lam . lam $ z $$ x 2
  , lam . lam . lam $ (lam z) $$ x 2
  , lam . lam . lam $ ((x 2) $$ z) $$ (s $$ z)
  , lam . lam . lam $ z $$ s $$ x 2
  , lam (lam (lam (lam (z $$ s $$ x 2 $$ x 3))))
  , lam . lam . lam . lam $ ((x 0 $$ x 2) $$ (x 1 $$ x 3))
  , lam . lam . lam . lam . lam . lam . lam . lam $
      ((x 0 $$ x 4) $$ (x 2 $$ x 6)) $$ ((x 1 $$ x 5) $$ (x 3 $$ x 7))

  ]
  where
      z = Zero
      s = Succ Zero
      x 0 = Zero
      x n = Succ (x (n-1))
      lam = Lam

-- NB: the choice of ordering (x2 x vs x x2) is what fixes the evaluation order
-- but we always do LTR for simplicity

-- distributeVars v (Shared s : vss) xx yy = Wn s ##
--   distributeVarsT True
--     (genericReplicate s OnRight ++ genericReplicate s OnLeft ++ vss)
--     (shift s 0 0 xx) (shift s s 0 yy)

-- distributeVars True [OnLeft] x y = flip ($) (countInitial OnLeft vs) $ \(i,vss) ->
--   Cn i ## distributeVarsT True vss x y
-- -- \x. x f -> C f
-- -- \x. (f x) g -> C f g
-- distributeVars _ vs@(OnLeft : _) x y = flip ($) (countInitial OnLeft vs) $ \(i,vss) ->
--   Cn i ## distributeVarsT True vss x y

-- -- \x. f x -> f
-- distributeVars True vs@(OnRight : _) x y = flip ($) (countInitial OnRight vs) $ \(i,vss) ->
--   distributeVarsI vss x y
-- -- \x. f (g x) -> B f g
-- distributeVars False vs@(OnRight : _) x y = flip ($) (countInitial OnRight vs) $ \(i,vss) ->
--   Bn i ## distributeVarsI vss x y

countInitial :: Sharing -> [Sharing] -> (Integer, [Sharing])
countInitial x (y:xs) | x == y = (\(a,b) -> (1 +a,b)) $ countInitial x xs
countInitial _ xs = (0,xs)

-- | shift terms up by s, preserving terms below height h, while inside l lambdas
-- NB: could use h-l instead, but this is more debuggable
shift ::Integer -> Integer -> Integer -> Deb -> Deb
shift s h l Zero = if l > h then nTimes s Succ Zero else Zero
shift s h l (Hask x) = Hask x
shift s h l (Succ c) = Succ (shift s (h-1) l c)
shift s h l (Lam ll) = Lam (shift s h (l+1) ll)
shift s h l (App x y) = App (shift s h l x) (shift s h l y)

splitVar :: Deb -> Integer -> Integer -> (Integer, Deb)
splitVar Zero c 0 = (c-1, nTimes c Succ Zero)
splitVar (Succ x) c l = second Succ $ splitVar x c (l-1)
splitVar (Lam d) c l = second Lam $ splitVar d c (l+1)
splitVar (App a b) c l = let
  (acnt, anew) = splitVar a c l
  (bcnt, bnew) = splitVar b acnt l
    in (bcnt, App anew bnew)
splitVar x c l = (c,x)

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