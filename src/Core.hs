{-# LANGUAGE DeriveFunctor #-}

import Control.Exception (assert)
import GHC.Stack (HasCallStack)

delete :: HasCallStack => Int -> [a] -> [a]
delete _ []     = error "delete: element not present"
delete 0 (y:ys) = ys
delete n (y:ys) = y : delete (n-1) ys


deleteM :: HasCallStack => [Int] -> [a] -> [a]
deleteM [] ys = ys
deleteM [x] ys = delete x ys
deleteM (x:xs@(xp:_)) ys = assert (x < xp) $ delete x (deleteM xs ys)

insertAt :: HasCallStack => Int -> a -> [a] -> [a]
insertAt 0 a ls = a : ls
insertAt n a [] = error "insertAt: list too short"
insertAt n a (l:ls) = l : insertAt (n-1) a ls

data Atom
    = Var String
    | Constant String
    deriving (Eq,Ord,Show)

data Form 
    = Atom Atom
    | Bot
    | Plus [Form]
    | Lolli [Form] [Form]
    | Bang Form
    | Quest Form
    deriving (Eq,Ord,Show)

data Seq = Seq [Form] [Form]
   deriving (Eq,Ord,Show)

seqHead (Seq a b) = a
seqTail (Seq a b) = b

data Rule r
  = Identity [Form]
  | Axiom Atom
  | Cut Int Int r r
  | ImplR [Int] [Int] r -- [Int] must be sorted ascending
  | ImplL [(Int,r)] [(Int,r)]
  | PlusL Int r [(Int,r)]
  | ZeroL Seq
  | PlusR Int Int [Form] r

  | WeakenL Form r
  | WeakenR Form r
  | ContractL Int Int r
  | ContractR Int Int r
  | BangL Int r
  | QuestR Int r
  | BangR Int r
  | QuestL Int r
    deriving (Eq,Ord,Show,Functor)

rule (Identity a) = Seq a a
rule (Axiom a) = Seq [Atom a] [Atom a]
rule (Cut a b (Seq w x) (Seq y z)) = assert (y !! a == x !! b) $ Seq (w ++ delete a y) (delete b x ++ z)
rule (ImplR a b (Seq x y))
    = Seq (deleteM a x) (deleteM b y ++ [Lolli (map (x!!) a) (map (y!!) b)])
rule (ImplL a b) =
    let
      ll = Lolli (map (\(i,r) -> seqTail r !! i) a) (map (\(i,r) -> seqHead r !! i) b)
      x = (a >>= seqHead . snd) ++ (b >>= (\(i,r) -> delete i (seqHead r)))
      y = (a >>= (\(i,r) -> delete i (seqTail r))) ++ (b >>= seqTail . snd)
    in
        Seq ([ll] ++ x) y

rule (PlusL a (Seq x y) ys) = assert (and . map ((delete a x == ) . seqHead . snd) $ ys) $
                              assert (and . map ((y==) . seqTail . snd) $ ys) $
                                Seq (delete a x ++ [Plus ([x !! a] ++ map (\(i,r) -> seqHead r !! i) ys)]) y
rule (ZeroL (Seq x y)) = Seq (x ++ [Plus []]) y
rule (PlusR i j cs (Seq x y)) = Seq x (delete i y ++ [Plus (insertAt j (y !! i) cs)])
rule (WeakenL a (Seq x y)) = Seq (x ++ [Bang a]) y
rule (WeakenR a (Seq x y)) = Seq x (y ++ [Quest a])
rule (ContractL a b (Seq x y)) =
    let af@(Bang _) = x !! a
        bf@(Bang _) = x !! b
    in
        assert (af == bf) $ Seq (delete b x) y
rule (ContractR a b (Seq x y)) =
    let af@(Quest _) = y !! a
        bf@(Quest _) = y !! b
    in
        assert (af == bf) $ Seq x (delete b y)
rule (BangL a (Seq x y)) = Seq ([Bang (x !! a)] ++ delete a x) y
rule (QuestR a (Seq x y)) = Seq x ([Quest (y !! a)] ++ delete a y)
rule (BangR a (Seq x y)) = Seq x ([Bang (y !! a)] ++ delete a y) -- assert: x is all Bang's, delete a y is all Quest's
rule (QuestL a (Seq x y)) = Seq ([Quest (x !! a)] ++ delete a x) y  -- assert: delete a x is all Bang's, y is all Quest's

-- C   -- Control
-- E   -- Environment
-- (S) -- Store
-- K   -- Continuation

data RuleExp = RE (Rule RuleExp)
  deriving (Show,Eq,Ord)

seqify :: RuleExp -> Seq
seqify (RE r) = rule $ fmap seqify r
{-

dual :: Form -> Form
dual a@(Atom _) = Lolli [a] []
dual (Bang e) = Quest (dual e)
dual (Quest e) = Bang (dual e)
dual (Lolli [a] []) = [a]
dual (Lolli [x] [Bot]) = [x]
dual (Lolli x y) = Lolli (map dual y) (map dual x)
dual x = Lolli x []

async Bot = True
async (Lolli _ _) = True
async (Quest _) = True
async _ = False


perp a = Lolli a []
par a b = Lolli (Perp a) b
tensor a b = Perp (Lolli a (Perp b))
with a b = Perp (Plus [Perp a,Perp b])
unit = Perp Bot
top = Perp (Plus [])

var = RE . Axiom . Var
lam s e = RE $ ImplR (Atom (Var s)) (head . seqTail . seqify $ e) e
app e f = RE $ ImplL (head . seqTail . seqify $ e) (head . seqTail . seqify $ f) e f
topret = RE . BotR . RE $ Identity [] -- Identity [Bot] ?
argeval e f = RE $ Cut (last . seqHead . seqify $ f) e f
funcall = argeval -- should be Cut as well with different argument polarities
-- Fun String Exp Env Kont
-}


{-

data Form 
    = Atom Atom
    | Perp Form
    | Bot
    | Plus [Form]
    | Lolli Form Form
    | Bang Form
    | Quest Form
    deriving (Eq,Ord,Show)



-- small-step semantics step
step :: State -> State
step s@(State c e k) = case c of
  Var v -> case e ! v of
    Closure v' b e' -> State (Lam v' b) e' k
  Ap cf cx -> State cf e (Arg cx e k)
  Lam v b -> case k of
    Top -> s
    Arg cx e' k' -> State cx e' (Fun v b e k')
    Fun v' b' e' k' -> State b' (extend v' (Closure v b e) e') k'





data State = State Exp Env Kont
  deriving Show

data Env = Env [(String,Value)]
  deriving Show

Env e ! v = case lookup v e of
    Nothing -> error "No value in env"
    Just x -> x

extend :: String -> Value -> Env -> Env
extend i v (Env f) = Env $ (i,v) : f

data Value = Closure Exp | Blackhole
  deriving Show

final :: State -> Bool
final (State Lam{} _ Top) = True
final _ = False

start :: Exp -> State
start c = State c (Env []) Top

id_ = Lam "x" $ Var "x"
const_ = Lam "x" $ Lam "y" $ Var "x"

-- until :: (a -> Bool) -> (a -> a) -> a -> a

eval :: State -> State
eval = until final step 

-- -}
{-

types - no logical meaning

command Cut - <v || k> a.k.a. Eval v k
Command MultiCut  - Let binding
command existsR Jump 'jump j sigma-vec v-vec' - alos handles tuples
binding cut v3 - rec{} - not necesssary per paper pg4 2.2.3 last 2 paragraphs
binding existsL - Label - mu-tilde - tuples and existsL
binding Name / optional-weaken-right (x = v)

Axiom: Term var 'x : t' / Continuation 'ret : t'
continuation case 'case-of alts'
continuation ->L = apply-lambda v . k
continuation forallL apply sigma . k
continuation TLK - K (b vec, x vec) -> c
Continuation Default - no-op type conversion 'x -> c'
term ->R '\x. v'
term forallR '\\a. v'
term TRK 'K (sigma vec, v vec)'
Term act - no-op type conversion 'mu ret. c'
-}

{-
-- | A general computation. A command brings together a list of bindings and
-- either:
--   * A computation to perform, including a /term/ that produces a value, some
--     /frames/ that process the value, and an /end/ that may finish the
--     computation or branch as a case expression.
--   * A jump to a join point, with a list of arguments and the join id.
data Command b = Let (Command b) (Command b)
               | Eval (Term b) [Frame b] (End b)
               | Jump [Term b] Id
               | Rec [Command b]
               | BindTerm b (Term b)
               | BindJoin b [b] (Command b)
  deriving (Functor, Foldable, Traversable)


type Program a  = [Command a]

-- | An expression producing a value. These include literals, lambdas,
-- and variables, as well as types and coercions (see GHC's 'GHC.Expr' for the
-- reasoning). They also include computed values, which bind the current
-- continuation in the body of a command.
data Term b     = Lit Literal       -- ^ A primitive literal value.
                | VarT Id            -- ^ A term variable. Must /not/ be a
                                    -- nullary constructor; use 'Cons' for this.
                | Lam b (Term b)    -- ^ A function. Binds some arguments and
                                    -- a continuation. The body is a command.
                | Compute Type (Command b)
                                    -- ^ A value produced by a computation.
                                    -- Binds a continuation.  The type is the type
                                    -- of the value returned by the computation
                | Type Type         -- ^ A type. Used to pass a type as an
                                    -- argument to a type-level lambda.
                | Coercion Coercion -- ^ A coercion. Used to pass evidence
                                    -- for the @cast@ operation to a lambda.
  deriving (Functor, Foldable, Traversable)

data Cont b = App (Arg b) (Cont b)
            | Cast Coercion (Cont b)
            | Tick (Tickish Id) (Cont b)
            | Return
            | Case b [(AltCon,[b],Command b)]
  deriving (Functor, Foldable, Traversable)

-}


