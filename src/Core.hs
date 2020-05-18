{-# LANGUAGE DeriveFunctor #-}

delete :: (Eq a) => a -> [a] -> [a]
delete _ []        = error "delete: element not present"
delete x (y:ys)    = if x == y then ys else y : delete x ys

deleteM :: (Eq a) => [a] -> [a] -> [a]
deleteM [] y = y
deleteM (x:xs) y = deleteM xs (delete x y)

data Atom
    = Var String
    | Constant String
    deriving (Eq,Ord,Show)

data Form 
    = Atom Atom
    | Perp Form
    | Bot
    | Plus [Form]
    | Lolli Form Form
    | Bang Form
    | Quest Form
    deriving (Eq,Ord,Show)

dual :: Form -> Form
dual a@(Atom _) = Perp a
dual (Perp a) = a
dual (Bang e) = Quest (dual e)
dual (Quest e) = Bang (dual e)
dual (Lolli x Bot) = x
dual (Lolli x y) = Lolli (dual y) (dual x)
dual x = Perp x

async Bot = True
async (Lolli _ _) = True
async (Quest _) = True
async (Perp a) = True
async _ = False

data Seq = Seq [Form] [Form]
   deriving (Eq,Ord,Show)

seqHead (Seq a b) = a
seqTail (Seq a b) = b

par a b = Lolli (Perp a) b
tensor a b = Perp (Lolli a (Perp b))
with a b = Perp (Plus [Perp a,Perp b])
unit = Perp Bot
top = Perp (Plus [])

data Rule r
  = Identity [Form]
  | Axiom Atom
  | Cut Form r r
  | PerpL Form r
  | PerpR Form r
  | PlusL [(Form,r)] Seq
  | PlusR Int [Form] r
  | BotR r
  | BotL
  | ImplR Form Form r
  | ImplL Form Form r r
  | WeakenL Form r
  | WeakenR Form r
  | ContractL Form r
  | ContractR Form r
  | BangL Form r
  | QuestR Form r
  | BangR Form r
  | QuestL Form r
    deriving (Eq,Ord,Show,Functor)

rule (Identity a) = Seq a a
rule (Axiom a) = Seq [Atom a] [Atom a]
rule (Cut a (Seq w x) (Seq y z)) = Seq (w ++ delete a y) (delete a x ++ z)
rule (PerpL a (Seq x y)) = Seq (x ++ [Perp a]) (delete a y)
rule (PerpR a (Seq x y)) = Seq (delete a x) ([Perp a] ++ y)
rule (PlusL as (Seq x y)) = Seq (x ++ [Plus (map fst as)]) y
  -- assert delete a s == x and the other side is y for each rule
rule (PlusR i cs (Seq x y)) = Seq x (delete (cs !! i) y ++ [Plus cs])
  -- assert a in cs
rule (BotR (Seq x y)) = Seq x (y ++ [Bot])
rule BotL = Seq [Bot] []
rule (ImplR a b (Seq x y))
    = Seq (delete a x) (delete b y ++ [Lolli a b])
rule (ImplL a b (Seq w x) (Seq y z))
    = Seq ([Lolli a b] ++ w ++ delete b y) (delete a x ++ z)
rule (WeakenL a (Seq x y)) = Seq (x ++ [Bang a]) y
rule (WeakenR a (Seq x y)) = Seq x (y ++ [Quest a])
rule (ContractL a (Seq x y)) =
    let aa = Bang a
        xx = [aa] ++ delete aa (delete aa x)
        in Seq xx y
rule (ContractR a (Seq x y)) =
    let aa = Quest a
        yy = [aa] ++ delete aa (delete aa y)
        in Seq x yy
rule (BangL a (Seq x y)) = Seq ([Bang a] ++ delete a x) y
rule (QuestR a (Seq x y)) = Seq x ([Quest a] ++ delete a y)
rule (BangR a (Seq x y)) = Seq x ([Bang a] ++ delete a y) -- assert: x is all Bang's, delete a y is all Quest's
rule (QuestL a (Seq x y)) = Seq ([Quest a] ++ delete a x) y  -- assert: delete a x is all Bang's, y is all Quest's

-- C   -- Control
-- E   -- Environment
-- (S) -- Store
-- K   -- Continuation

data RuleExp = RE (Rule RuleExp)
  deriving (Show,Eq,Ord)

seqify :: RuleExp -> Seq
seqify (RE r) = rule $ fmap seqify r

var = RE . Axiom . Var
lam s e = RE $ ImplR (Atom (Var s)) (head . seqTail . seqify $ e) e
app e f = RE $ ImplL (head . seqTail . seqify $ e) (head . seqTail . seqify $ f) e f
topret = RE . BotR . RE $ Identity [] -- Identity [Bot] ?
argeval e f = RE $ Cut (last . seqHead . seqify $ f) e f
funcall = argeval -- should be Cut as well with different argument polarities
-- Fun String Exp Env Kont



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


