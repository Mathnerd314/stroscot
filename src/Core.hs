{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable, TupleSections, RecordWildCards #-}

import Prelude hiding (Either,Left,Right)
import Control.Exception (assert)
import GHC.Stack (HasCallStack)
import Data.Function (on)
import Data.List (groupBy,sortBy,find,sort,zipWith4,transpose,findIndex,elemIndex)
import Data.Maybe(fromJust)
import Control.Arrow((&&&))
import Control.Monad (forM_)
import Control.Monad.State
import Debug.Trace(trace,traceShow,traceM,traceShowM)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Data

newtype Tag = Tag String
   deriving (Eq,Ord,Show)
newtype EID = EID String
   deriving (Eq,Ord,Show)
newtype Level = Level Integer
   deriving (Eq,Ord,Show)
newtype Var = Var String
   deriving (Eq,Ord,Show)

type Gamma = [EID]
type Delta = [EID]
type Theta = [EID]
type Lambda = [EID]
type A = EID
type B = EID
type T = EID
type I = Level
type J = Level

data Sequent left right = Sequent
  { turnstile :: EID
  , left :: left
  , right :: right
  }
   deriving (Eq,Ord,Show)

data Rule top bottom = Rule
  { top :: top
  , bottom :: bottom
  }
   deriving (Eq,Ord,Show)

-- compare with glue/core_explicit.hs which has slots for all of the contexts
data Syntax
  = PiR (Rule
      [(Tag, Sequent (Gamma, [A]) ([B], Delta))]
      (Sequent Gamma (T, Delta))
      ) -- contexts kept because there are multiple case (phi node)
  | PiL Tag (Rule
      ([Sequent () A], [Sequent B ()])
      (Sequent T ())
      )
  | SigmaR Tag (Rule
      ([Sequent B ()], [Sequent () A])
      (Sequent () T)
      )
  | SigmaL (Rule
      [(Tag, Sequent (Gamma, [A]) ([B], Delta))]
      (Sequent (Gamma, T) Delta)
      ) -- contexts kept because there are multiple case (phi node)
  | Bang I J (Rule
      (Sequent Gamma (A, Delta))
      (Sequent Gamma (T, Delta))
      ) -- contexts kept because it's a box
  | BangD I J (Rule
      (Sequent A ())
      (Sequent T ())
      )
  | BangC (Rule
      (Sequent [T] ())
      (Sequent T ())
      )
  | BangW (Rule
      (Sequent () ())
      (Sequent T ())
      )
  | Whim I J (Rule
      (Sequent (Gamma, A) Delta)
      (Sequent (Gamma, T) Delta)
      ) -- contexts kept because it's a box
  | WhimD I J (Rule
      (Sequent () A)
      (Sequent () T)
      )
  | WhimC (Rule
      (Sequent () [T])
      (Sequent () T)
      )
  | WhimW (Rule
      (Sequent () ())
      (Sequent () T)
      )
  | Identity (Rule () (Sequent A A))
  | Cut (Rule
      (Sequent () A, Sequent A ())
      (Sequent () ())
      )
  | Use Var (Rule () (Sequent Gamma Delta)) -- contexts kept because they're bindings
  | Assign Var (Rule (Sequent Gamma Delta) ()) -- contexts kept because they're bindings
   deriving (Eq,Ord,Show)

data VDir = Top | Bottom
   deriving (Eq,Ord,Show)
data HDir = Left | Right | Turnstile
   deriving (Eq,Ord,Show)

ports :: Syntax -> [(VDir,(HDir,EID))]
ports s =
  case s of
    PiR r -> rp r
    PiL _ r -> rp r
    SigmaR _ r -> rp r
    SigmaL r -> rp r
    Bang _ _ r -> rp r
    BangD _ _ r -> rp r
    BangC r -> rp r
    BangW r -> rp r
    Whim _ _ r -> rp r
    WhimD _ _ r -> rp r
    WhimC r -> rp r
    WhimW r -> rp r
    Identity r -> rp r
    Cut r -> rp r
    Use _ r -> rp r
    Assign _ r -> rp r
  where
    rp (Rule {..}) = map (Top,) (getHPorts top) ++ map (Bottom,) (getHPorts bottom)

class GetHPorts s where
  getHPorts :: s -> [(HDir,EID)]
instance (GetPorts a, GetPorts b) => GetHPorts (Sequent a b) where
  getHPorts (Sequent{..}) = [(Turnstile,turnstile)] ++ map (Left,) (getPorts left) ++ map (Right,) (getPorts right)
instance (GetHPorts a, GetHPorts b) => GetHPorts (a, b) where
  getHPorts (a,b) = getHPorts a ++ getHPorts b
instance (GetHPorts a) => GetHPorts [a] where
  getHPorts = concatMap getHPorts
instance GetHPorts () where
  getHPorts () = []
instance GetHPorts Tag where
  getHPorts _ = []

class GetPorts s where
  getPorts :: s -> [EID]
instance GetPorts EID where
  getPorts = pure
instance GetPorts () where
  getPorts () = []
instance (GetPorts a, GetPorts b) => GetPorts (a, b) where
  getPorts (a,b) = getPorts a ++ getPorts b
instance (GetPorts a) => GetPorts [a] where
  getPorts = concatMap getPorts

type Graph = [Rule]

data Lam = App Lam Lam | Lam String Lam | Var String

freevars (Var v) = Set.singleton v
freevars (Lam v b) = Set.delete v (freevars b)
freevars (App a b) = freevars a `Set.union` freevars b

-- delta (\h. delta (h I))
delta v = Lam v (App (Var v) (Var v))
m = App (delta "x")
    (Lam "h" (App (delta "y")
      (App (Var "h") (Lam "i" (Var "i")))))

example_m :: Graph
example_m =
  [ Assign (Var "m") (Rule (Sequent (EID "retseq") [] [EID "rettop"]) ())
  , Identity (Rule () (Sequent (EID "idseq") (EID "ret") (EID "rettop")))
  ] ++ translate (EID "ret") (EID "idseq") (EID "retseq") m

translate :: EID -> EID -> EID -> Lam -> Graph
translate s (Var v) = [
  BangD I J (Rule
    (Sequent v ())
    (Sequent s ())
  )
BangD v s]
translate s (Lam v b) = let
  sr = s ++ "r"
  vr = v ++ "r"
  vl = v ++ "l"
  in
    [Cut sr s
    ,PiRight sr ec [("func",Ctx [v] [vr],ec)]
    ,Identity vl vr
    ] ++ translate vl b
translate s (App a b) = let
  s1 = s ++ "1"; s2 = s ++ "2"; sp = s ++ "p"; si = s ++ "i"
  av = freevars a
  bv = freevars b
  shared = av `Set.intersection` bv
  replaceOne shared n v = if v `Set.member` shared then v ++ n else v
  replace shared n f = fmap (replaceOne shared n) f
  at = map (replace shared "1") $ translate s1 a
  bt = map (replace bv "j") $ translate s2 b
  in
    [ PiLeft s1 "func" [sp] [s]
    ] ++ at ++ map (\v -> BangCW v [v ++ "1", v ++ "2"]) (Set.toList shared) ++
    [ Bang sp (Ctx (replace shared "2" $ Set.toList bv) []) si (Ctx (replace bv "j" $ Set.toList bv) [])
    , Identity s2 si
    ] ++ bt
