{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable, TupleSections, RecordWildCards, FlexibleInstances #-}

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
import Data.Tuple (swap)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Data
import Data.Char(isDigit)

newtype Tag = Tag String
   deriving (Eq,Ord,Show)
newtype EID = EID String
   deriving (Eq,Ord,Show)
newtype Level = Level Integer
   deriving (Eq,Ord,Show)
newtype Var = VID String
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
type AI = (I, EID)
type AJ = (J, EID)
type TI = (I, EID)
type TJ = (J, EID)
type GammaI = [(I,EID)]
type DeltaI = [(I,EID)]

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
      ) -- contexts kept because there are multiple cases (phi node)
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
      ) -- contexts kept because there are multiple cases (phi node)
  | Bang (Rule
      (Sequent GammaI (AJ, DeltaI))
      (Sequent GammaI (TI, DeltaI))
      ) -- contexts kept because it's a box
  | BangD (Rule
      (Sequent AI ())
      (Sequent TJ ())
      )
  | BangC (Rule
      (Sequent [T] ())
      (Sequent T ())
      )
  | BangW (Rule
      (Sequent () ())
      (Sequent T ())
      )
  | Whim (Rule
      (Sequent (GammaI, AJ) DeltaI)
      (Sequent (GammaI, TI) DeltaI)
      ) -- contexts kept because it's a box
  | WhimD (Rule
      (Sequent () AI)
      (Sequent () TJ)
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

type Graph = [Syntax]

data Lam = App Lam Lam | Lam String Lam | Var String

freevars (Var v) = Set.singleton v
freevars (Lam v b) = Set.delete v (freevars b)
freevars (App a b) = freevars a `Set.union` freevars b

-- delta (\h. delta (h I))
delta v = Lam v (App (Var v) (Var v))
mx = App (delta "x")
    (Lam "h" (App (delta "y")
      (App (Var "h") (Lam "i" (Var "i")))))

m = Lam "x" (Var "x")

mkEID :: String -> State Int EID
mkEID s = do
  n <- get
  put (n+1)
  return . EID $ s ++ show n

mkedge = mkEID "e"

infixl 7 %%
(EID xn) %% s = mkEID (x ++ s) where
  x = reverse . dropWhile isDigit . reverse $ xn

example_m :: State Int Graph
example_m = do
  retseq <- mkEID "retseq"
  ret <- mkEID "ret"
  tr <- translate (Level 0) Map.empty retseq ret m
  return $ tr ++ [ Assign (VID "m") (Rule (Sequent retseq [] [ret]) ())]

translate :: Level -> Map String (Level, EID) -> EID -> EID -> Lam -> State Int Graph
translate retlvl vars seq ret (Var v) = do
  vseq <- mkEID $ v ++ "seq"
  vret <- mkEID $ v ++ "ret"
  return $ [Identity (Rule () (Sequent vseq vret ret))
    ,BangD (Rule
      (Sequent vseq (retlvl, vret) ())
      (Sequent seq (fromJust $ Map.lookup v vars) ())
    )]
translate retlvl vars seq ret (Lam v b) = do
  varsi <- let act s (l,v) = swap $ runState (do
                e <- v %% "i"
                return (l,e)) s in
      state $ \s -> swap $ Map.mapAccum act s vars
  nseq <- mkEID "seq"
  vedge <- mkEID v
  nret <- mkEID "ret"
  (nseq2, a) <- if v `Set.member` freevars b then return (nseq, []) else do
      nseq2 <- mkEID "seq"
      return (nseq2, [BangW (Rule
        (Sequent nseq2 () ())
        (Sequent nseq vedge ())
        )])
  tr <- translate retlvl (Map.insert v (retlvl, vedge) varsi) nseq nret b
  return $ tr ++ a ++ [ PiR (Rule
      [(Tag "func", Sequent nseq (map snd $ Map.elems varsi, [vedge]) ([nret], []))]
      (Sequent seq (map snd $ Map.elems vars) (ret, []))
      )]
{-
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
Cut sr s,
    ,Identity vl vr
    [ Bang sp (Ctx (replace shared "2" $ Set.toList bv) []) si (Ctx (replace bv "j" $ Set.toList bv) [])
    , Identity s2 si
    ] ++ bt
-}


data VDir = Top | Bottom
   deriving (Eq,Ord,Show)
data HDir = Left | Right | Turnstile
   deriving (Eq,Ord,Show)

data PathE = Index Int | Fst | Snd
    deriving (Eq,Ord,Show)

data GD = GD
  { eid :: EID
  , side :: HDir
  , level :: Maybe Level
  , sequent_path :: [PathE]
  , outer_path :: [PathE]
  }
   deriving (Eq,Ord,Show)

ports :: Syntax -> [(VDir,GD)]
ports s =
  case s of
    PiR r -> rp r
    PiL _ r -> rp r
    SigmaR _ r -> rp r
    SigmaL r -> rp r
    Bang r -> rp r
    BangD r -> rp r
    BangC r -> rp r
    BangW r -> rp r
    Whim r -> rp r
    WhimD r -> rp r
    WhimC r -> rp r
    WhimW r -> rp r
    Identity r -> rp r
    Cut r -> rp r
    Use _ r -> rp r
    Assign _ r -> rp r
  where
    rp (Rule {..}) = map (Top,) (getHPorts top) ++ map (Bottom,) (getHPorts bottom)

addPath p = map (\gd -> gd { sequent_path = p : sequent_path gd})
addHPath p = map (\gd -> gd { outer_path = p : outer_path gd})

class GetHPorts s where
  getHPorts :: s -> [GD]
instance (GetPorts a, GetPorts b) => GetHPorts (Sequent a b) where
  getHPorts (Sequent{..}) =
    [t_gd turnstile] ++
    map (\gd -> gd { side = Left}) (getPorts left) ++
    map (\gd -> gd { side = Right}) (getPorts right)
     where
       t_gd eid = GD eid Turnstile Nothing [] []
instance (GetHPorts a, GetHPorts b) => GetHPorts (a, b) where
  getHPorts (a,b) = addHPath Fst (getHPorts a) ++ addHPath Snd (getHPorts b)
instance (GetHPorts a) => GetHPorts [a] where
  getHPorts xs = concat $ zipWith (\n -> addHPath (Index n)) [0..] (map getHPorts xs)
instance GetHPorts () where
  getHPorts () = []
instance GetHPorts Tag where
  getHPorts _ = []

class GetPorts s where
  getPorts :: s -> [GD]
instance GetPorts EID where
  getPorts e = [GD e undefined Nothing [] []]
instance{-# OVERLAPPING  #-}  GetPorts (Level, EID) where
  getPorts (l,e) = [GD e undefined (Just l) [] []]
instance GetPorts () where
  getPorts () = []
instance {-# OVERLAPPABLE #-} (GetPorts a, GetPorts b) => GetPorts (a, b) where
  getPorts (a,b) = addPath Fst (getPorts a) ++ addPath Snd (getPorts b)
instance (GetPorts a) => GetPorts [a] where
  getPorts xs = concat $ zipWith (\n -> addPath (Index n)) [0..] (map getPorts xs)

tagName :: Syntax -> String
tagName s =
  case s of
    PiR _ -> "PiR"
    PiL _ _ -> "PiL"
    SigmaR _ _ -> "SigmaR"
    SigmaL _ -> "SigmaL"
    Bang _ -> "Bang"
    BangD _ -> "BangD"
    BangC _ -> "BangC"
    BangW _ -> "BangW"
    Whim _ -> "Whim"
    WhimD _ -> "WhimD"
    WhimC _ -> "WhimC"
    WhimW _ -> "WhimW"
    Identity _ -> "Identity"
    Cut _ -> "Cut"
    Use _ _ -> "Use"
    Assign _ _ -> "Assign"

data EdgeInfo = EdgeInfo
  { e_eid :: EID
  , e_side :: HDir
  , e_top_port :: Int
  , e_bot_port :: Int
  , e_level :: Maybe Level
  }
   deriving (Eq,Ord,Show)

combine :: HasCallStack => (Int,GD) -> (Int,GD) -> EdgeInfo
combine (top_port,a) (bottom_port,b) = EdgeInfo
  { e_eid = assert (eid a == eid b) $ eid a
  , e_side = assert (side a == side b) $ side a
  , e_top_port = top_port
  , e_bot_port = bottom_port
  , e_level = j (level a) (level b)
  }
  where
    j :: (Eq a) => Maybe a -> Maybe a -> Maybe a
    j = joinMaybes
    joinMaybes (Just a) (Just b) = assert (a == b) (Just a)
    joinMaybes (Just a) Nothing = Just a
    joinMaybes Nothing (Just b) = Just b
    joinMaybes Nothing Nothing = Nothing

-- > groupSortOn length ["test","of","sized","item"] == [["of"],["test","item"],["sized"]]
groupSortOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupSortOn f = map (map snd) . groupBy ((==) `on` fst) . sortBy (compare `on` fst) . map (f &&& id)

addPortNumbers :: [(VDir,GD)] -> [(Int, (VDir,GD))]
addPortNumbers ps = let
  key (vd,gd) = (vd,side gd)
  grouped = groupSortOn key ps
  in
    concat $ map (zip [0..]) grouped

type Edge = (Int,Int,EdgeInfo)
type Edges = [Edge]
edges :: Graph -> Edges
edges rs = let
  ps = concat $ zipWith (\x -> map (x,)) [0..] $ map (addPortNumbers . ports) rs
  ess = groupSortOn (\(_,(_,(_,gd))) -> eid gd) ps
  buildEdge es = case es of
    [(i,(pa,(Top,sa))),(j,(pb,(Bottom,sb)))] -> (i,j,combine (pa,sa) (pa,sb))
    [(j,(pb,(Bottom,sb))),(i,(pa,(Top,sa)))] -> (i,j,combine (pa,sa) (pa,sb))
    _ -> error $ show es
  in map buildEdge ess

type Names = Map Syntax String

nodeNames :: Graph -> Names
nodeNames rs = let
  rss = groupSortOn tagName rs
  in Map.fromList (rss >>= zipWith (\n x -> (x,tagName x ++ show n)) [0..])

quote n = "\"" ++ n ++ "\""

nodes :: Names -> Syntax -> Syntax -> [String]
nodes names actN r = [buildNode n (r == actN)]
  where
    n = fromJust (Map.lookup r names)
    buildNode n b = quote n ++ " [label=" ++ quote (tagName r) ++
      (if b then ",shape=doublecircle" else "") ++ "]\n"

edgeOut :: Names -> Graph -> Set EID -> Edge -> String
edgeOut names graph activeE (i,j,einf) = let
  getN n = fromJust (Map.lookup (graph !! n) names)
  ax = getN i
  bx = getN j
  getEIDStr (EID e) = e
  (a,b,color,weight,arrowhead,arrowtail) = case e_side einf of
    Left ->  (bx,ax,"blue,constraint=false",0,",arrowtail=",",arrowhead=")
    Right -> (ax,bx,"red",2,",arrowhead=",",arrowtail=")
    Turnstile -> (ax,bx,"black",5,",arrowhead=",",arrowtail=")
  ahead = arrowhead ++ case e_bot_port einf of
    0 -> "normal"
    1 -> "onormal"
    2 -> "dot"
    3 -> "odot"
    4 -> "diamond"
    5 -> "odiamond"
    6 -> "box"
    7 -> "obox"
  atail = arrowtail ++ case e_top_port einf of
    0 -> "none"
    1 -> "inv"
    2 -> "oinv"
    3 -> "linv"
    4 -> "rinv"
    5 -> "olinv"
    6 -> "orinv"
    7 -> "crow"
  in
    quote a ++ " -> " ++ quote b
      ++ "[color=" ++ color ++ ",weight=" ++ show weight
      ++ ",tooltip=" ++ quote (getEIDStr $ e_eid einf)
      ++ ahead ++ atail
      ++ (if e_eid einf `Set.member` activeE then ",penwidth=2" else "")
      ++ ",dir=both"
      ++ "]\n"

mkDot :: [Syntax] -> [(EID, Syntax)] -> String
mkDot graph active = let
  names = nodeNames graph
  activeE = Set.fromList $ map fst active
  activeN = snd $ last active
  nodetxt = concat (graph >>= nodes names activeN)
  edgetxt = edges graph >>= edgeOut names graph activeE
  in
    "digraph {\nrankdir=\"BT\"\n" ++ nodetxt ++ edgetxt ++ "}\n"

writeFirst = do
  let graph = fst $ runState example_m 0
  let f = mkDot graph [(EID "ret1",last graph)]
  writeFile "graphs/test.dot" f
