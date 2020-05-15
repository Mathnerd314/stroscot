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

data Exp 
    = Atom Atom
    | Perp Exp
    | Bot
    | Plus [Exp]
    | Lolli Exp Exp
    | Bang Exp
    | Quest Exp
    deriving (Eq,Ord,Show)

dual :: Exp -> Exp
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
async (Perp a) = not (async a)
async _ = False

data Seq = Seq [Exp] [Exp]
   deriving (Eq,Ord,Show)

par a b = Lolli (Perp a) b
tensor a b = Perp (Lolli a (Perp b))
with a b = Perp (Plus [Perp a,Perp b])
unit = Perp Bot
top = Perp (Plus [])

data Rule r
  = Identity [Exp]
  | Axiom Atom
  | Cut Exp r r
--  | MultiCut [Exp] [Exp] Seq Seq
  | PerpL Exp r
  | PerpR Exp r
  | PlusL [(Exp,r)] Seq
  | PlusR Exp [Exp] r
  | BotR r
  | BotL
  | ImplR Exp Exp r
  | ImplL Exp Exp r r
  | WeakenL Exp r
  | WeakenR Exp r
  | ContractL Exp r
  | ContractR Exp r
  | BangL Exp r
  | QuestR Exp r
  | BangR Exp r
  | QuestL Exp r
    deriving (Eq,Ord,Show)

rule (Identity a) = Seq a a
rule (Axiom a) = Seq [Atom a] [Atom a]
rule (Cut a (Seq w x) (Seq y z)) = Seq (w ++ delete a y) (delete a x ++ z)
-- rule (MultiCut as bs (Seq w x) (Seq y z)) = Seq (deleteM bs w ++ deleteM as y) (deleteM as x ++ deleteM bs z)
rule (PerpL a (Seq x y)) = Seq (x ++ [Perp a]) (delete a y)
rule (PerpR a (Seq x y)) = Seq (delete a x) ([Perp a] ++ y)
rule (PlusL as (Seq x y)) = Seq (x ++ [Plus (map fst as)]) y
  -- assert delete a s == x and the other side is y for each rule
rule (PlusR a cs (Seq x y))= Seq x (delete a y ++ [Plus cs])
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

{-

Axiom: Term var / Continuation Ret
command Cut
Command MultiCut/Let(rec)
continuation Case
binding Rec
term -> R
continuation -> L
term forall R
continuation forall L
command Jump
binding Label
term TRK
continuation TLK
binding Name / weaken right

Term act - no-op type conversion
Continuation Default - no-op type conversion


-- | An entire program.
type Program a  = [Bind a]

-- | A binding. Similar to the @Bind@ datatype from GHC. Can be either a single
-- non-recursive binding or a mutually recursive block.
data Bind b     = NonRec (BindPair b) -- ^ A single non-recursive binding.
                | Rec [BindPair b]    -- ^ A block of mutually recursive bindings.
  deriving (Functor, Foldable, Traversable)

-- | The binding of one identifier to one term or continuation.
data BindPair b = BindTerm b (Term b)
                | BindJoin b (Join b)
  deriving (Functor, Foldable, Traversable)

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

-- | A parameterized continuation, otherwise known as a join point. Where a
-- regular continuation represents the context of a single expression, a
-- join point is a point in the control flow that many different computations
-- might jump to.
data Join b     = Join [b] (Command b)
  deriving (Functor, Foldable, Traversable)


-- | A general computation. A command brings together a list of bindings and
-- either:
--   * A computation to perform, including a /term/ that produces a value, some
--     /frames/ that process the value, and an /end/ that may finish the
--     computation or branch as a case expression.
--   * A jump to a join point, with a list of arguments and the join id.
data Command b = Let (Bind b) (Command b)
               | Eval (Term b) [Frame b] (End b)
               | Jump [Term b] Id
  deriving (Functor, Foldable, Traversable)

-- | A piece of context for a term. So called because we can think of them as
-- as stack frames in an idealized abstract machine. Typically a list of frames
-- is simply a list of arguments, but other bits of context such as casts and
-- ticks are also frames.
--
-- In contrast to an 'End', a 'Frame' takes input and produces output. Thus a
-- 'Command' can be seen as a pipeline starting from a 'Term', passing through
-- some 'Frame's, and ending at an 'End'.
data Frame b    = App {- expr -} (Arg b)
                  -- ^ Apply the value to an argument.
                | Cast {- expr -} Coercion
                  -- ^ Cast the value using the given coercion.
                | Tick (Tickish Id) {- expr -}
                  -- ^ Annotate the enclosed frame. Used by the profiler.
  deriving (Functor, Foldable, Traversable)

-- | The end of a continuation. After arguments and casts are applied, we can
-- do two things to a value: Return it to the calling context or perform a case
-- analysis.
data End b      = Return
                  -- ^ Pass to the bound continuation.
                | Case {- expr -} b [Alt b]
                  -- ^ Perform case analysis on the value.
  deriving (Functor, Foldable, Traversable)

-- | A case alternative. Given by the head constructor (or literal), a list of
-- bound variables (empty for a literal), and the body as a 'Command'.
data Alt b      = Alt AltCon [b] (Command b)
  deriving (Functor, Foldable, Traversable)

-- | The frames and end from an Eval, together forming a continuation.
type Kont b     = ([Frame b], End b)
                  -- ^ A continuation is expressed as a series of frames
                  -- (usually arguments to apply), followed by a terminal
                  -- action (such as a case analysis).
-- -}
