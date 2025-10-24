{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable, TupleSections, RecordWildCards, FlexibleInstances, BangPatterns, LambdaCase, ImplicitParams #-}

import Prelude hiding (Either,Left,Right)
import Control.Exception (assert)
import GHC.Stack (HasCallStack)
import Data.Function (on)
import Data.List hiding (delete)
import Data.Maybe(fromJust)
import Control.Arrow((&&&))
import Control.Monad (forM_)
import Control.Monad.State.Lazy
import Control.Monad.Writer
import Data.Traversable(traverse, for)
import qualified Data.Functor.Identity as FIdentity ( Identity(..) )
import Debug.Trace
import Data.Tuple (swap)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Data
import Data.Char(isDigit)
import System.Process
import qualified Data.Either as E

------------------
-- Core data types

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
-- type I = Level
-- type J = Level
type AI = EID
type AJ = EID
type TI = EID
type TJ = EID
type GammaI = [(EID)]
type DeltaI = [(EID)]

data SequentX turnstile left right = Sequent
  { turnstile :: turnstile
  , left :: left
  , right :: right
  }
   deriving (Eq,Ord,Show)

type Sequent = SequentX EID
type SequentI = SequentX (EID)

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
      ) -- contexts kept because there are multiple pr_cases (phi node)
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
      ) -- contexts kept because there are multiple pr_cases (phi node)
  | Bang (Rule
      (SequentI GammaI (AJ, DeltaI))
      (SequentI GammaI (TI, DeltaI))
      ) -- indexed, contexts kept because it's a box
  | BangD (Rule
      (SequentI AI ())
      (SequentI TJ ())
      ) -- indexed
  | BangC (Rule
      (Sequent [T] ())
      (Sequent T ())
      )
  | BangW (Rule
      (Sequent () ())
      (Sequent T ())
      )
  | Whim (Rule
      (SequentI (GammaI, AJ) DeltaI)
      (SequentI (GammaI, TI) DeltaI)
      ) -- indexed, contexts kept because it's a box
  | WhimD (Rule
      (SequentI () AI)
      (SequentI () TJ)
      ) -- indexed
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
  | Dup HDir (Rule EID [EID])
  | DupInv HDir (Rule [EID] EID)
   deriving (Eq,Ord,Show)

type Graph = [Syntax]

mkEID :: String -> State Int EID
mkEID s = do
  n <- get
  put (n+1)
  return . EID $ s ++ show n

mkedge :: State Int EID
mkedge = mkEID "e"

infixl 7 %%
(%%) :: EID -> [Char] -> State Int EID
(EID xn) %% s = mkEID (x ++ s) where
  x = reverse . dropWhile isDigit . reverse $ xn

freshen :: [Char] -> (a, EID) -> State Int (a, EID)
freshen s (l,v) = do
  e <- v %% s
  return (l,e)

------------------
-- Lambda calculus

data Lam = App Lam Lam | Lam String Lam | Var String

freevars :: Lam -> Set String
freevars (Var v) = Set.singleton v
freevars (Lam v b) = Set.delete v (freevars b)
freevars (App a b) = freevars a `Set.union` freevars b

translate :: EID -> Map String (Level, EID) -> (Level, EID) -> Lam -> State Int Graph
translate seq vars (retlvl,ret) (Var v) = do
  vseq <- mkEID $ v ++ "seq"
  vret <- mkEID $ v ++ "ret"
  let vl@(i,_) = vars Map.! v
  return $ [Identity (Rule () (Sequent vseq vret ret))
    ,BangD (Rule
      (Sequent vseq vret ())
      (Sequent seq (snd vl) ())
    )]
translate seq vars (retlvl,ret) (Lam v b) = do
  varsi <- traverse (freshen "i") vars
  nseq <- mkEID "seq"
  vedge <- mkEID v
  nret <- mkEID "ret"
  (nseq2, a, nvars) <- if v `Set.member` freevars b then
      return (nseq, [], Map.insert v (retlvl, vedge) varsi)
    else do
      nseq2 <- mkEID "seq"
      return (nseq2, [BangW (Rule
        (Sequent nseq2 () ())
        (Sequent nseq vedge ())
        )], varsi)
  tr <- translate nseq nvars (retlvl,nret) b
  return $ tr ++ a ++ [ PiR (Rule
      [(Tag "func", Sequent nseq (map snd $ Map.elems varsi, [vedge]) ([nret], []))]
      (Sequent seq (map snd $ Map.elems vars) (ret, []))
      )]
translate seq vars (retlvl,ret) (App a b) = do
  let av = Map.fromSet (vars Map.!) $ freevars a
  let bv = Map.fromSet (vars Map.!) $ freevars b
  let shared = av `Map.intersection` bv
  sharedA <- traverse (freshen "A") shared
  sharedB <- traverse (freshen "B") shared
  bangseqs <- replicateM (Map.size shared) (mkEID "seq")
  let bangcs = zipWith5 (\st sb b_t a b -> BangC (Rule
                  (Sequent st [a,b] ())
                  (Sequent sb b_t ())
                )) bangseqs (seq:bangseqs) (map snd $ Map.elems shared) (map snd $ Map.elems sharedA) (map snd $ Map.elems sharedB)
  let lastbangseq = last (seq:bangseqs)
  c_lseq <- mkEID "c_lseq"; c_rseq <- mkEID "c_rseq"
  lval <- mkEID "lval"; rval <- mkEID "rval"
  let cut = Cut (Rule
        (Sequent c_rseq () rval, Sequent c_lseq lval ())
        (Sequent lastbangseq () ())
        )
  at <- translate c_rseq (sharedA `Map.union` av) (retlvl,rval) a
  newrret <- mkEID "newrret"; lret <- mkEID "lret"
  dseq <- mkEID "dseq"; idseq <- mkEID "idseq"
  let pil = PiL (Tag "func") (Rule
            ([Sequent dseq () newrret], [Sequent idseq lret ()])
            (Sequent c_lseq lval ())
            )
  let ident = Identity (Rule () (Sequent idseq lret ret))
  let bvars = sharedB `Map.union` bv
  bvarsf <- traverse (freshen "f") bvars
  newrretf <- newrret %% "f"; c_bseq <- mkEID "c_bseq"
  let retlvlinc = case retlvl of Level i -> Level (i+1)
  let bang = Bang (Rule
              (Sequent c_bseq (map snd $ Map.elems bvarsf) (newrretf, []))
              (Sequent dseq (map snd $ Map.elems bvars) (newrret, []))
              )
  bt <- translate c_bseq bvarsf (retlvlinc,newrretf) b
  return $ bt ++ [bang] ++ [ident] ++ [pil] ++ at ++ [cut] ++ bangcs

----------------
-- Graph display

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
    Dup hd r -> map (\(v,gd) -> (v,gd {side = hd, level= Nothing})) (rp r)
    DupInv hd r ->  map (\(v,gd) -> (v,gd {side = hd, level = Nothing})) (rp r)
  where
    rp Rule {..} = map (Top,) (getHPorts top) ++ map (Bottom,) (getHPorts bottom)

addPath :: PathE -> [GD] -> [GD]
addPath p = map (\gd -> gd { sequent_path = p : sequent_path gd})
addHPath :: PathE -> [GD] -> [GD]
addHPath p = map (\gd -> gd { outer_path = p : outer_path gd })

class GetHPorts s where
  getHPorts :: s -> [GD]
instance {-# OVERLAPPABLE #-} (GetPorts a, GetPorts b) => GetHPorts (Sequent a b) where
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
instance GetHPorts EID where
  getHPorts e = [GD e undefined Nothing [] []]
instance{-# OVERLAPPING #-} GetHPorts (Level, EID) where
  getHPorts (l,e) = [GD e undefined (Just l) [] []]

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

tagName :: (?lambdaStyle :: Bool) => Syntax -> String
tagName s =
  case s of
    PiR _ | ?lambdaStyle -> "λ"
    PiL _ _ | ?lambdaStyle -> "@"
    PiR _ -> "PiR"
    PiL _ _ -> "PiL"
    SigmaR _ _ -> "SigmaR"
    SigmaL _ -> "SigmaL"
    Bang _ | ?lambdaStyle -> "!p"
    BangD _ | ?lambdaStyle -> "!d"
    BangC _ | ?lambdaStyle -> "!c"
    BangW _ | ?lambdaStyle -> "!w"
    Bang _ -> "Bang"
    BangD _ -> "BangD"
    BangC _ -> "BangC"
    BangW _ -> "BangW"
    Whim _ -> "Whim"
    WhimD _ -> "WhimD"
    WhimC _ -> "WhimC"
    WhimW _ -> "WhimW"
    Identity _ | ?lambdaStyle -> "I"
               | otherwise -> "Identity"
    Cut _ -> "Cut"
    Use _ _ -> "Use"
    Assign _ _ -> "Assign"
    Dup _ _ -> "Dup"
    DupInv _ _ -> "DupI"

data EdgeInfo = EdgeInfo
  { e_eid :: EID
  , e_side :: HDir
  , e_top_port :: Int
  , e_bot_port :: Int
  , e_level :: Maybe Level
  }
   deriving (Eq,Ord,Show)

combine :: HasCallStack => (Int,GD) -> (Int,GD) -> EdgeInfo
combine (top_port,a) (bottom_port,b) =
  let r = EdgeInfo
        { e_eid = assert (eid a == eid b) $ eid a
        , e_side = assert (side a == side b) $ side a
        , e_top_port = top_port
        , e_bot_port = bottom_port
        , e_level = j (level a) (level b)
        } in r
  where
    j :: (Eq a, Show a) => Maybe a -> Maybe a -> Maybe a
    j = joinMaybes
    joinMaybes (Just x) (Just y) | x == y = (Just x)
                                 | otherwise = traceShow (eid a, x, y) (Just x)
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
    [(i,(pa,(Top,sa))),(j,(pb,(Bottom,sb)))] -> [(i,j,combine (pa,sa) (pb,sb))]
    [(j,(pb,(Bottom,sb))),(i,(pa,(Top,sa)))] -> [(i,j,combine (pa,sa) (pb,sb))]
    _ -> trace ("mismatched edge: " ++ show es) []
  in concatMap buildEdge ess

type Names = Map Syntax String


nodeNames :: (?lambdaStyle :: Bool) => Graph -> Names
nodeNames rs = Map.fromList $ zipWith (\n x -> (x,tagName x ++ show n)) [0..] rs

quote :: [Char] -> [Char]
quote n = "\"" ++ n ++ "\""

nodes :: (?lambdaStyle :: Bool) => Names -> Syntax -> Syntax -> [String]
nodes names actN r = [buildNode n (r == actN)]
  where
    n = fromJust (Map.lookup r names)
    buildNode n b = quote n ++ " [label=" ++ quote (tagName r) ++
      (if b then ",shape=doublecircle" else "") ++ "]\n"

edgeOut :: (?lambdaStyle :: Bool) =>Names -> Graph -> Set EID -> Edge -> String
edgeOut names graph activeE (i,j,einf) | ?lambdaStyle && e_side einf == Turnstile = ""
edgeOut names graph activeE (i,j,einf) = let
  getN n = fromJust (Map.lookup (graph !! n) names)
  ax = getN i
  bx = getN j
  getEIDStr (EID e) = e
  (a,b,color,weight,arrowhead,arrowtail) = case e_side einf of
    Left | ?lambdaStyle ->  (bx,ax,"blue",1,",arrowtail=",",arrowhead=")
         | otherwise   ->  (ax,bx,"blue",2,",arrowhead=",",arrowtail=")
    Right -> (ax,bx,"red",2,",arrowhead=",",arrowtail=")
    Turnstile -> (ax,bx,"black",5,",arrowhead=",",arrowtail=")
    -- Left ->  (bx,ax,"blue",1,",arrowtail=",",arrowhead=")
    -- Right -> (ax,bx,"red",5,",arrowhead=",",arrowtail=")
    -- Turnstile -> (ax,bx,"black,constraint=false",0,",arrowhead=",",arrowtail=")
  ahead = arrowhead ++ case e_bot_port einf of
    0 -> "normal"
    1 -> "onormal"
    2 -> "dot"
    3 -> "odot"
    4 -> "diamond"
    5 -> "odiamond"
    6 -> "box"
    7 -> "obox"
    _ -> undefined
  atail = arrowtail ++ case e_top_port einf of
    0 -> "none"
    1 -> "inv"
    2 -> "oinv"
    3 -> "linv"
    4 -> "rinv"
    5 -> "olinv"
    6 -> "orinv"
    7 -> "crow"
    _ -> undefined
  in
    quote a ++ " -> " ++ quote b
      ++ "[color=" ++ color ++ ",weight=" ++ show weight
      ++ ",tooltip=" ++ quote (getEIDStr $ e_eid einf)
      ++ case e_level einf of Nothing -> ""; Just (Level l) -> ",label=" ++ quote (show l)
      ++ ahead ++ atail
      ++ (if e_eid einf `Set.member` activeE then ",penwidth=2" else "")
      ++ ",dir=both"
      ++ "]\n"

mkDot :: (?lambdaStyle :: Bool) => [Syntax] -> [(Edge, Syntax)] -> String
mkDot graph active = let
  names = nodeNames graph
  activeE = Set.fromList $ map (\((_,_,eid),_) -> e_eid eid) active
  activeN = snd $ last active
  nodetxt = concat (graph >>= nodes names activeN)
  edgetxt = edges graph >>= edgeOut names graph activeE
  in
    "digraph {\nrankdir=\"" ++ (if ?lambdaStyle then "TB" else "BT") ++ "\"\n" ++ nodetxt ++ edgetxt ++ "}\n"

-----------------------
-- Graph transformation

findEdge edges en = find (\(_,_,einf) -> en == e_eid einf) edges

data GDir = Down | Up
   deriving (Eq,Ord,Show)

findNode :: [a] -> (Int, Int, c) -> GDir -> a
findNode graph e@(from,to,gd) Up = graph !! from
findNode graph e@(from,to,gd) Down = graph !! to

get_from_id :: (a, b, c) -> a
get_from_id (from,to,gd) = from
get_gd :: (a, b, c) -> c
get_gd (_,_,gd) = gd

delete :: HasCallStack => Int -> [a] -> [a]
delete _ []     = error "delete: element not present"
delete 0 (y:ys) = ys
delete n (y:ys) = y : delete (n-1) ys

-- [Int] must be sorted ascending
deleteM :: HasCallStack => [Int] -> [a] -> [a]
deleteM [] ys = ys
deleteM [x] ys = delete x ys
deleteM (x:xs@(xp:_)) ys = assert (x < xp) $ delete x (deleteM xs ys)

class TraverseEID s where
  traverseEID :: (Applicative f) => (EID -> f EID) -> s -> f s
instance TraverseEID Syntax where
  traverseEID f s = case s of
    PiR r -> PiR <$> traverseEID f r
    PiL t r -> PiL t <$> traverseEID f r
    SigmaR t r -> SigmaR t <$> traverseEID f r
    SigmaL r -> SigmaL <$> traverseEID f r
    Bang r -> Bang <$> traverseEID f r
    BangD r -> BangD <$> traverseEID f r
    BangC r -> BangC <$> traverseEID f r
    BangW r -> BangW <$> traverseEID f r
    Whim r -> Whim <$> traverseEID f r
    WhimD r -> WhimD <$> traverseEID f r
    WhimC r -> WhimC <$> traverseEID f r
    WhimW r -> WhimW <$> traverseEID f r
    Identity r -> Identity <$> traverseEID f r
    Cut r -> Cut <$> traverseEID f r
    Use t r -> Use t <$> traverseEID f r
    Assign t r -> Assign t <$> traverseEID f r
    Dup h r -> Dup h <$> traverseEID f r
    DupInv h r -> DupInv h <$> traverseEID f r
instance (TraverseEID a, TraverseEID b) => TraverseEID (Rule a b) where
  traverseEID f (Rule t b) = Rule <$> traverseEID f t <*> traverseEID f b
instance (TraverseEID a, TraverseEID b) => TraverseEID (Sequent a b) where
  traverseEID f (Sequent{..}) = Sequent <$> traverseEID f turnstile <*> traverseEID f left <*> traverseEID f right
instance (TraverseEID a, TraverseEID b) => TraverseEID (a, b) where
  traverseEID f (a,b) = (,) <$> traverseEID f a <*> traverseEID f b
instance (TraverseEID a) => TraverseEID [a] where
  traverseEID f xs = traverse (traverseEID f) xs
instance TraverseEID () where
  traverseEID f () = pure ()
instance TraverseEID Tag where
  traverseEID f x = pure x
instance TraverseEID EID where
  traverseEID f e = f e
instance TraverseEID Level where
  traverseEID f l = pure l

fmapEID :: TraverseEID c => (EID -> EID) -> c -> c
fmapEID f = FIdentity.runIdentity . traverseEID (FIdentity.Identity . f)

reduce :: Graph -> Edges -> Edge -> GDir -> ([(Edge,Syntax)], State Int Graph)
reduce graph edges e@(from_id,to_id,einf) dir = let
  r = findNode graph e dir
  s = e_side einf

  follow e d = do
    ei <- findEdge edges e
    pure $ findNode graph ei d

  replaceM :: State Int [Syntax] -> ([(Edge,Syntax)], State Int Graph)
  replaceM s = ([(e,r)], s)

  replace :: [Syntax] -> [Syntax] -> Graph -> Graph
  replace del add graph = filter (\x -> not (x `elem` del)) graph ++ add

  -- join [(top,bottom)] replaces all top with bottom
  join_edges :: [(EID,EID)] -> Graph -> Graph
  join_edges replacements graph = let
    replacements_map = Map.fromList replacements
    change :: EID -> EID
    change b = case Map.lookup b replacements_map of
      Just bn -> change bn
      Nothing -> b
    in fmapEID change graph

  expand en dn = case findEdge edges en of
    Nothing -> ([], pure graph)
    Just edge_n -> let (rest, g) = reduce graph edges edge_n dn in ((e,r) : rest, g)
  in case (r,s,dir) of
  (Dup hd (Rule dup_out dup_ins), _, _) ->
    case follow dup_out Down of
      Nothing -> ([], pure graph)
      Just dup_target ->
        case dup_target of
          Dup _ _ | dir == Down -> expand dup_out Down
          DupInv hdi (Rule dupi_ins dupi_out) | hd == hdi -> replaceM $ do
            pure . join_edges (zip dupi_ins dup_ins) . replace [r,dup_target] [] $ graph
          _ -> replaceM $ do
            (newnodes, newedges) <- fmap unzip . for dup_ins $ \in_edge -> runWriterT $ traverseEID (\e ->
                if e == dup_out then pure in_edge else do
                  e' <- lift $ e %% ""
                  tell [(e,e')]
                  pure e') dup_target
            -- make dup/dupinv nodes
            let t_ports = filter (\(_,gd) -> eid gd /= dup_out) $ ports dup_target
            let tr_edges = transpose newedges
            let f (v,gd) out_edges = case v of {
              Top -> Dup (side gd) (Rule (eid gd) (map (\(e,e') -> assert (e == eid gd) $ e') out_edges))
            ; Bottom -> DupInv (side gd) (flip Rule (eid gd) (map (\(e,e') -> assert (e == eid gd) $ e') out_edges))
            }
            let newdupnodes = assert (length t_ports == length tr_edges) $ zipWith f t_ports tr_edges
            pure $ replace [r,dup_target] (newnodes++newdupnodes) graph
  (DupInv hd (Rule dup_ins dup_out), _, Up) -> expand dup_out Up
{-
    case follow lbi Up of
      Nothing -> ([], pure graph)
      Just lift_target ->
        case lift_target of
          DupInv _ _ _ | dir == Up -> expand lbi Up
          Lift hd (Rule (f,lt) (g,lb)) | lt == lbi && hd == hdi && f == i && j == g -> replaceM $ do
            pure . join_edges ([(lti,lb)]) . replace [r,lift_target] [] $ graph
          _ -> replaceM $ do
            (newnode, newedges) <- runWriterT $ traverseEID (\e ->
                if e == lbi then pure lti else do
                  e' <- lift $ e %% ""
                  tell [(e,e')]
                  pure e') lift_target
            -- make dup/dupinv nodes
            let t_ports = filter (\(_,gd) -> eid gd /= lbi) $ ports lift_target
            let f (v,gd) (e,e') = let !() = assert (e == eid gd) () in traceShow (v,gd,e') $ case v of {
              Top -> Lift (side gd) (Rule (i,e) (j,e'))
            ; Bottom -> LiftInv (side gd) (Rule (j,e') (i,e))
            }
            let newliftnodes = assert (length t_ports == length newedges) $ zipWith f t_ports newedges
            pure $ replace [r,lift_target] ([newnode]++newliftnodes) graph
-}
  (Cut (Rule (Sequent c_rseq () c_r, Sequent c_lseq c_l ()) (Sequent c_bseq () ())), _, Up)->
    case follow c_r Down of
      Nothing -> ([], pure graph)
      Just right_node ->
        case follow c_l Down of
          Nothing -> ([], pure graph)
          Just left_node ->
            case (right_node,left_node) of
              (_, Identity (Rule () (Sequent iseq ill irr))) | ill == c_l -> replaceM $ do
                pure . join_edges [(irr,c_r),(c_rseq,iseq),(c_lseq,c_bseq){-,(c_l,ill)-}] . replace [r,left_node] [] $ graph
              (Identity (Rule () (Sequent iseq ill irr)), _) | irr == c_r -> replaceM $ do
                pure . join_edges [(ill,c_l),(c_lseq,iseq),(c_rseq,c_bseq){-,(c_r,irr)-}] . replace [r,right_node] [] $ graph
              (Dup _ _, _) -> expand c_r Down
              (_, Dup _ _) -> expand c_l Down
              (PiR (Rule pr_cases (Sequent pr_bseq pr_bl (pr_main, pr_br))),
                PiL t (Rule (pl_r, pl_l) (Sequent pl_bseq pl_main ()))) | c_r == pr_main && pl_main == c_l -> replaceM $ do
                let ([(_,Sequent pr_m_seq (pr_m_tl, pr_m_l) (pr_m_r, pr_m_tr))], extraCases@[]) = partition ((==t) . fst) pr_cases
                -- todo: delete extraCases
                let !() = assert (length pl_r == length pr_m_l) ()
                let !() = assert (length pl_l == length pr_m_r) ()
                seqP <- replicateM (length pl_l + length pl_r - 1) (mkEID "seqP") -- TODO: handle lengths (0,0)
                let seqs = [pr_m_seq] ++ seqP ++ [pr_bseq]

                let mkCutLL pr_m_r pl_l tseq bseq = Cut (Rule (Sequent tseq () pr_m_r,  Sequent (turnstile $ pl_l) (left $ pl_l) ()) (Sequent bseq () ()))
                let mkCutLR pl_r pr_m_l tseq bseq = Cut (Rule (Sequent (turnstile $ pl_r) () (right $ pl_r), Sequent tseq pr_m_l ()) (Sequent bseq () ()))
                let newcuts = zipWith3 id (zipWith mkCutLR pl_r pr_m_l ++ zipWith mkCutLL pr_m_r pl_l) seqs (tail seqs)

                pure . join_edges ([(c_lseq, c_bseq), (c_rseq, pl_bseq)] ++ zip pr_m_tl pr_bl ++ zip pr_m_tr pr_br) .
                  replace [r,right_node,left_node] newcuts $ graph
              (Bang (Rule (Sequent b_tseq b_tl (b_tmain, b_tr)) (Sequent b_bseq b_bl (b_bmain, b_br))),
                BangC (Rule (Sequent bc_tseq bc_t ()) (Sequent bc_bseq bc_b ()))) | c_r == b_bmain && c_l == bc_b -> replaceM $ do
                let u = undefined
                bangs <- traverseEID (%% "") $ replicate (length bc_t) right_node
                let f (Bang (Rule (Sequent b_tseq b_tl (b_tmain, b_tr)) (Sequent b_bseq b_bl (b_bmain, b_br))))
                      (bangs_tseq, bangs_tl, bangs_tmain, bangs_tr, bangs_bseq, bangs_bl, bangs_bmain, bangs_br) =
                              (b_tseq:bangs_tseq, b_tl:bangs_tl, b_tmain:bangs_tmain, b_tr:bangs_tr,
                              b_bseq:bangs_bseq, b_bl:bangs_bl, b_bmain:bangs_bmain, b_br:bangs_br)
                    f _ _ = undefined

                let (bangs_tseq, bangs_tl, bangs_tmain, bangs_tr, bangs_bseq, bangs_bl, bangs_bmain, bangs_br)
                      = foldr f ([],[],[],[],[],[],[],[]) bangs

                let dup_seq = Dup Turnstile (Rule (b_tseq) (bangs_tseq))
                let dup_l = zipWith (\b c -> Dup Left (Rule b c)) (b_tl) (transpose $ bangs_tl)
                let dup_r = zipWith (\b c -> Dup Right (Rule b c)) (b_tr) (transpose $ bangs_tr)
                let dup_main = Dup Right (Rule (b_tmain) (bangs_tmain))

                cutseq <- replicateM (length bc_t - 1) (mkEID "seqBC")
                whimseq <- replicateM (length b_br) (mkEID "seqBW")
                bangseq <- replicateM (length b_bl) (mkEID "seqBB")
                let seqs = [bc_tseq] ++ cutseq ++ whimseq ++ bangseq ++ [b_bseq]

                let mkCut bangs_bseq bangs_bmain bc_t tseq bseq =
                      Cut (Rule (Sequent bangs_bseq () bangs_bmain, Sequent tseq bc_t ()) (Sequent bseq () ()))
                let cuts = zipWith3 mkCut (bangs_bseq) (bangs_bmain) bc_t
                let mkBangC b_bl bangs_bl tseq bseq = BangC (Rule (Sequent tseq bangs_bl ()) (Sequent bseq b_bl ()))
                let bangcs = zipWith mkBangC (b_bl) (transpose $ bangs_bl)
                let mkWhimC b_br bangs_br tseq bseq = WhimC (Rule (Sequent tseq () bangs_br) (Sequent bseq () b_br))
                let whimcs = zipWith mkWhimC (b_br) (transpose $ bangs_br)

                let spine = zipWith3 id (cuts ++ whimcs ++ bangcs) seqs (tail seqs)

                pure . join_edges [(c_lseq, c_bseq), (c_rseq, bc_bseq)] $
                  replace [r,right_node,left_node] ([dup_seq,dup_main] ++ dup_l ++ dup_r ++ bangs ++ spine) graph
              (Bang (Rule (Sequent b_tseq b_tl (b_tmain, b_tr)) (Sequent b_bseq b_bl (b_bmain, b_br))),
                BangD (Rule (Sequent bd_tseq bd_t ()) (Sequent bd_bseq bd_b ()))) | c_r == b_bmain && c_l == bd_b -> replaceM $ do
                let mkLift dir a b = E.Right (b,a)
                b_lift_tseq <- mkEID "seqDS"
                b_lift_tmain <- mkEID "seqDM"
                bd_lift_tseq <- mkEID "seqDD"
                bd_lift_t <- mkEID "seqDE"

                let lift_seq_l = mkLift Turnstile bd_tseq (bd_lift_tseq)
                let lift_main_l = mkLift Left bd_t (bd_lift_t)
                let lift_seq_r = mkLift Turnstile b_tseq (b_lift_tseq)
                let lift_main_r = mkLift Right b_tmain (b_lift_tmain)
                let lift_l = zipWith (mkLift Left) b_tl b_bl
                let lift_r = zipWith (mkLift Right) b_tr b_br

                let lifts_all = [lift_seq_l,lift_seq_r,lift_main_l,lift_main_r] ++ lift_l ++ lift_r
                let (lifts, joins) = E.partitionEithers lifts_all

                let cut = Cut (Rule (Sequent b_lift_tseq () b_lift_tmain, Sequent bd_lift_tseq bd_lift_t ()) (Sequent (b_bseq) () ()))

                pure . join_edges ([(c_lseq, c_bseq), (c_rseq, bd_bseq)] ++ joins) $
                  replace [r,right_node,left_node] (cut:lifts) graph

              (Bang (Rule (Sequent br_tseq br_tl (br_tmain, br_tr)) (Sequent br_bseq br_bl (br_bmain, br_br))),
                Bang (Rule (Sequent bl_tseq bl_tl (bl_tmain, bl_tr)) (Sequent bl_bseq bl_bl (bl_bmain, bl_br)))) | c_r == br_bmain -> replaceM $ do
                let n = fromJust $ findIndex (\eid -> eid == c_l) bl_bl
                let bl_tln = bl_tl !! n
                let
                  notn :: [a] -> [a]
                  notn l = let (h,t) = splitAt n l in h ++ tail t
                let bl_tlnotn = notn bl_tl
                let bl_blnotn = notn bl_bl
                let ilength = length br_bl
                let jlength = length br_br

                newcut_rseq <- mkEID "newcut_rseq"
                newcut_bseq <- mkEID "newcut_bseq"
                brl_bl_eid <- replicateM ilength (mkEID "brl_bl")
                brl_br_eid <- replicateM jlength (mkEID "brl_br")
                let brl_bl = brl_bl_eid
                let brl_br = brl_br_eid

                let bang_lower = Bang (Rule
                                  (Sequent br_tseq br_tl (br_tmain, br_tr))
                                  (Sequent newcut_rseq brl_bl (br_bmain, brl_br)))
                let newcut = Cut (Rule (Sequent newcut_rseq () (br_bmain), Sequent (bl_tseq) (bl_tln) ()) (Sequent newcut_bseq () ()))
                let bang_upper = Bang (Rule
                                  (Sequent (newcut_bseq) (bl_tlnotn++brl_bl) (bl_tmain, bl_tr ++ brl_br))
                                  (Sequent bl_bseq (bl_blnotn++br_bl) (bl_bmain, bl_br ++ br_br)))

                pure . join_edges ([(c_bseq, c_rseq), (br_bseq, c_lseq)]) $
                  replace [r,right_node,left_node] [bang_lower,newcut,bang_upper] graph

              (_,_) -> replaceM $ traceShow (r,right_node,left_node) undefined
  (Identity (Rule () (Sequent _ c_l _)),Right,Down) -> expand c_l Up
  (Identity (Rule () (Sequent _ _ c_r)),Left,Down) -> expand c_r Up
  (p,_,Up) -> expand i Up
    where
      i = case p of
          (PiR (Rule _ (Sequent _ _ (i, _)))) -> i
          (PiL _ (Rule _ (Sequent _ i _))) -> i
          (SigmaR _ (Rule _ (Sequent _ _ i))) -> i
          (SigmaL (Rule _  (Sequent _ (_, i) _))) -> i
          (Bang (Rule _ (Sequent _ _ ((i), _)))) -> i
        -- todo: handle auxiliary ports. Although we will never encounter them during reduction?
          (BangD (Rule _ (Sequent _ (i) ()))) -> i
          (BangC (Rule _ (Sequent _ i ()))) -> i
          (BangW (Rule _  (Sequent _ i ()))) -> i
          (Whim (Rule _  (Sequent _ (_, (i)) _))) -> i
          (WhimD (Rule _ (Sequent _ () (i)))) -> i
          (WhimC (Rule _ (Sequent _ () i))) -> i
          (WhimW (Rule _ (Sequent _ () i))) -> i
          _ -> traceShow p undefined
  x -> replaceM $ traceShow x undefined

---------------
-- Main program

-- delta (\h. delta (h I))
delta v = Lam v (App (Var v) (Var v))
mx = App (delta "x")
    (Lam "h" (App (delta "y")
      (App (Var "h") (Lam "i" (Var "i")))))

m = mx

example_m :: State Int Graph
example_m = do
  retseq <- mkEID "retseq"
  ret <- mkEID "ret"
  tr <- translate retseq Map.empty (Level 0, ret) m
  return $ tr ++ [ Assign (VID "m") (Rule (Sequent retseq [] [ret]) ())]

print_m :: IO ()
print_m = mapM_ print $ fst $ runState example_m 0

writeFirst :: IO ()
writeFirst = do
  let graph = fst $ runState example_m 0
  let f = let ?lambdaStyle = False in mkDot graph [(fromJust $ findEdge (edges graph) (EID "ret1"),last graph)]
  writeFile "graphs/test.dot" f

findRoot graph = case find (\case Assign _ _ -> True; _ -> False) graph of
  Just (Assign _ (Rule (Sequent _ [] [ret]) ())) -> ret
  _ -> error $ "no Assign?!"

iterGraph
  :: Graph -> [Maybe (EID, GDir)] -> State Int [([Syntax],[(Edge, Syntax)])]
iterGraph graph [] = pure []
iterGraph graph (r:rs) = do
  let edgesG = edges graph
  let (redex,redex_dir) =
                          case r of
                            Just (redex,redex_dir) -> (redex,redex_dir)
                            Nothing -> (findRoot graph, Down)
  let redex_edge = fromJust $ findEdge edgesG redex
  let (active, next) = reduce graph edgesG redex_edge redex_dir
  next_graph <- next
  rest <- iterGraph next_graph rs
  pure $ (graph, active) : rest

toDot :: Graph -> [Maybe (EID, GDir)] -> State Int [(String,String)]
toDot graph rs = do
  graphs <- iterGraph graph rs
  pure $ map (\(g,a) -> (let ?lambdaStyle = False in mkDot g a, let ?lambdaStyle = True in mkDot g a)) graphs

-- stroscot/Asperti_M_reduction.png

writeGraphs :: State Int Graph -> String -> Integer -> IO ()
writeGraphs graph name limit = do
  -- let Root b_t = fromJust $ find ((=="Root") . tagName) graph
  let rs = map (\x -> Just (EID x, Up))
          [ "rval71" -- 0
          , "rval15" -- 1
          , "rval49" -- 2
          , "rval5" -- 3, Asperti 1
          , "lret28" -- 4
          , "x7" -- 5
          , "xA9" -- 6
          , "xB10" -- 7
          , "xBf22" -- 8
          , "xret26" -- 9
          , "rval39" -- 10, Asperti 2
          , "ret42" -- 11
          , "newrret61" -- 12
          , "yA43" -- 13
          , "newrret117" -- 14
          , "yBf56" -- 15
          , "yret60" -- 16
          , "newrretf31" -- 17, Asperti 3
          , "c_bseq32" -- 18
          , "lval14" -- 19, Asperti 4
          , "lret19" -- 20
          , "hf65" -- 21
          , "c_bseq67" -- 22
          , "lval70" -- 23, Asperti 5
          , "hseq72" -- 24
          , "newrret74" -- 25
          , "dseq76" -- 26
          , "newrretf78" -- 27, Asperti 6
          , "c_bseq79" -- 28
          , "i81" -- 29
          , "seq80" -- 30
          , "iret84" -- 31
          , "ret82" -- 32, Asperti 7
          , "iseq83" -- 33
          , "lret75" -- 34
          , "newrretf66" -- 35
          , "idseq77" -- 36
          , "lval48" -- 37
          , "h34" -- 38, Asperti 8
          , "seq33" -- 39
          ] ++ [Nothing]

{-
          , Nothing -- 10, Asperti 3
          , Just (EID "c_bseq86", Down) -- 11
          , Nothing -- 12
          , Just (EID "seqDS95", Down) -- 13
          , Nothing -- 14
          , Nothing -- 15
          , Nothing -- 16
          , Nothing -- 17
          , Just (EID "y41", Up) -- 18
-}
            -- Nothing, -- 0
            -- Nothing,
            -- Nothing,
            -- Just (EID "lval38", Up), -- 3
            -- Nothing,
            -- Nothing,
            -- Nothing,
            -- Just (EID "c_bseq87", Down), -- 7
            -- Nothing,
            -- Just (EID "seqDS96",Down), -- 9
            -- Nothing,
            --  -- 11
            -- Nothing,
            -- Just (EID "lret62", Up), -- 13
            --  -- 14
            -- Nothing,
            -- Nothing,
            -- Just (EID "hf120", Down), -- 19
            -- Just (EID "c_bseq119", Down), -- 20
            -- Just (EID "lval138", Down), -- 21
            -- Nothing

  let graphs = fst $ flip runState 0 $ do
                g <- graph
                toDot g rs
  let out = zip [0..limit] graphs
  system $ "rm graphs/" ++ name ++ "_*.svg"
  forM_ out $ \(i,g) -> do
    putStrLn (show i)
    let gname = "graphs/" ++ name ++ "_" ++ show i
    writeFile (gname ++ ".dot") (fst g)
    system $ "dot -Tsvg " ++ gname ++ ".dot > " ++ gname ++ ".svg"
    writeFile (gname ++ "_l.dot") (snd g)
    system $ "dot -Tsvg " ++ gname ++ "_l.dot > " ++ gname ++ "_l.svg"

main :: IO ()
main = writeGraphs example_m "example_m" 1000
