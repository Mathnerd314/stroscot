{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable, TupleSections, RecordWildCards, FlexibleInstances #-}

import Prelude hiding (Either,Left,Right)
import Control.Exception (assert)
import GHC.Stack (HasCallStack)
import Data.Function (on)
import Data.List hiding (delete)
import Data.Maybe(fromJust)
import Control.Arrow((&&&))
import Control.Monad (forM_)
import Control.Monad.State
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
  | Dup Level HDir (Rule EID [EID])
  | DupInv Level HDir (Rule [EID] EID)
  | Lift HDir (Rule (Level,EID) (Level,EID))
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

m = mx

example_m :: State Int Graph
example_m = do
  retseq <- mkEID "retseq"
  ret <- mkEID "ret"
  tr <- translate retseq Map.empty (Level 0, ret) m
  return $ tr ++ [ Assign (VID "m") (Rule (Sequent retseq [] [ret]) ())]

mkEID :: String -> State Int EID
mkEID s = do
  n <- get
  put (n+1)
  return . EID $ s ++ show n

mkedge = mkEID "e"

infixl 7 %%
(EID xn) %% s = mkEID (x ++ s) where
  x = reverse . dropWhile isDigit . reverse $ xn

freshen s (l,v) = do
  e <- v %% s
  return (l,e)

translate :: EID -> Map String (Level, EID) -> (Level, EID) -> Lam -> State Int Graph
translate seq vars (retlvl,ret) (Var v) = do
  vseq <- mkEID $ v ++ "seq"
  vret <- mkEID $ v ++ "ret"
  return $ [Identity (Rule () (Sequent vseq vret ret))
    ,BangD (Rule
      (Sequent vseq (retlvl, vret) ())
      (Sequent seq (vars Map.! v) ())
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
  let bangcs = zipWith5 (\st sb o a b -> BangC (Rule
                  (Sequent st [a,b] ())
                  (Sequent sb o ())
                )) bangseqs (seq:bangseqs) (map snd $ Map.elems shared) (map snd $ Map.elems sharedA) (map snd $ Map.elems sharedB)
  let lastbangseq = last (seq:bangseqs)
  lseq <- mkEID "lseq"; rseq <- mkEID "rseq"
  lval <- mkEID "lval"; rval <- mkEID "rval"
  let cut = Cut (Rule
        (Sequent rseq () rval, Sequent lseq lval ())
        (Sequent lastbangseq () ())
        )
  at <- translate rseq (sharedA `Map.union` av) (retlvl,rval) a
  newrret <- mkEID "newrret"; lret <- mkEID "lret"
  dseq <- mkEID "dseq"; idseq <- mkEID "idseq"
  let pil = PiL (Tag "func") (Rule
            ([Sequent dseq () newrret], [Sequent idseq lret ()])
            (Sequent lseq lval ())
            )
  let ident = Identity (Rule () (Sequent idseq lret ret))
  let bvars = sharedB `Map.union` bv
  bvarsf <- traverse (freshen "f") bvars
  newrretf <- newrret %% "f"; bseq <- mkEID "bseq"
  let retlvlinc = case retlvl of Level i -> Level (i+1)
  let bang = Bang (Rule
              (Sequent bseq (Map.elems bvarsf) ((retlvlinc, newrretf), []))
              (Sequent dseq (Map.elems bvars) ((retlvl, newrret), []))
              )
  bt <- translate bseq bvarsf (retlvlinc,newrretf) b
  return $ bt ++ [bang] ++ [ident] ++ [pil] ++ at ++ [cut] ++ bangcs

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
    Dup _ hd r -> map (\(v,gd) -> (v,gd {side = hd})) (rp r)
    DupInv _ hd r ->  map (\(v,gd) -> (v,gd {side = hd})) (rp r)
    Lift hd r -> map (\(v,gd) -> (v,gd {side = hd})) (rp r)
  where
    rp (Rule {..}) = map (Top,) (getHPorts top) ++ map (Bottom,) (getHPorts bottom)

addPath p = map (\gd -> gd { sequent_path = p : sequent_path gd})
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

tagName :: Syntax -> String
tagName s =
  case s of
    PiR _ | lambdaStyle -> "λ"
    PiL _ _ | lambdaStyle -> "@"
    PiR _ -> "PiR"
    PiL _ _ -> "PiL"
    SigmaR _ _ -> "SigmaR"
    SigmaL _ -> "SigmaL"
    Bang _ | lambdaStyle -> "!p"
    BangD _ | lambdaStyle -> "!d"
    BangC _ | lambdaStyle -> "!c"
    BangW _ | lambdaStyle -> "!w"
    Bang _ -> "Bang"
    BangD _ -> "BangD"
    BangC _ -> "BangC"
    BangW _ -> "BangW"
    Whim _ -> "Whim"
    WhimD _ -> "WhimD"
    WhimC _ -> "WhimC"
    WhimW _ -> "WhimW"
    Identity _ | lambdaStyle -> "I"
               | otherwise -> "Identity"
    Cut _ -> "Cut"
    Use _ _ -> "Use"
    Assign _ _ -> "Assign"
    Dup l _ _ -> "Dup" ++ show l
    DupInv l _ _ -> "DupI" ++ show l
    Lift _ _ -> "Lift"

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
        } in traceShow r r
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
    [(i,(pa,(Top,sa))),(j,(pb,(Bottom,sb)))] -> (i,j,combine (pa,sa) (pb,sb))
    [(j,(pb,(Bottom,sb))),(i,(pa,(Top,sa)))] -> (i,j,combine (pa,sa) (pb,sb))
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

lambdaStyle = True

edgeOut :: Names -> Graph -> Set EID -> Edge -> String
edgeOut names graph activeE (i,j,einf) | lambdaStyle && e_side einf == Turnstile = ""
edgeOut names graph activeE (i,j,einf) = let
  getN n = fromJust (Map.lookup (graph !! n) names)
  ax = getN i
  bx = getN j
  getEIDStr (EID e) = e
  (a,b,color,weight,arrowhead,arrowtail) = case e_side einf of
    Left | lambdaStyle ->  (bx,ax,"blue",1,",arrowtail=",",arrowhead=")
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
    "digraph {\nrankdir=\"" ++ (if lambdaStyle then "TB" else "BT") ++ "\"\n" ++ nodetxt ++ edgetxt ++ "}\n"

writeFirst = do
  let graph = fst $ runState example_m 0
  let f = mkDot graph [(EID "ret1",last graph)]
  writeFile "graphs/test.dot" f

findEdge edges en = fromJust $ find (\(_,_,einf) -> en == e_eid einf) edges

data GDir = Down | Up
   deriving (Eq,Ord,Show)

findNode graph e@(from,to,gd) Up = graph !! from
findNode graph e@(from,to,gd) Down = graph !! to

get_from_id (from,to,gd) = from
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
    Dup l h r -> Dup l h <$> traverseEID f r
    DupInv l h r -> DupInv l h <$> traverseEID f r
    Lift h r -> Lift h <$> traverseEID f r
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

fmapEID f = FIdentity.runIdentity . traverseEID (FIdentity.Identity . f)

reduce :: Graph -> Edges -> Edge -> GDir -> ([(Edge,Syntax)], State Int Graph)
reduce graph edges e@(from_id,to_id,einf) dir = let
  r = findNode graph e dir
  s = e_side einf

  follow e = findNode graph (findEdge edges e)

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

  expand en dn = let (rest, g) = reduce graph edges (findEdge edges en) dn in ((e,r) : rest, g)
  in case (r,s,dir) of
  (Dup l hd (Rule dup_out dup_ins), _, Down) -> let
    dup_target = follow dup_out Down
    in case dup_target of
      Dup _ _ _ | dir == Down -> expand dup_out Down
      DupInv li hdi (Rule dupi_ins dupi_out) | l == li && hd == hdi -> replaceM $ do
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
        let f (v,gd) out_edges = traceShow (v,gd,out_edges) $ case v of {
          Top -> Dup l (side gd) (Rule (eid gd) (map (\(e,e') -> assert (e == eid gd) $ e') out_edges))
        ; Bottom -> DupInv l (side gd) (flip Rule (eid gd) (map (\(e,e') -> assert (e == eid gd) $ e') out_edges))
        }
        let newdupnodes = assert (length t_ports == length tr_edges) $ zipWith f t_ports tr_edges
        pure $ replace [r,dup_target] (newnodes++newdupnodes) graph
  (Cut (Rule (Sequent rseq () rr, Sequent lseq ll ()) (Sequent bseq () ())), _, Up)-> let
    right_node = follow rr Down
    left_node = follow ll Down
    in case (right_node,left_node) of
      (_, Identity (Rule () (Sequent iseq ill irr))) | ill == ll -> replaceM $ do
        pure . join_edges [(irr,rr),(rseq,iseq),(lseq,bseq){-,(ll,ill)-}] . replace [r,left_node] [] $ graph
      (Identity (Rule () (Sequent iseq ill irr)), _) | irr == rr -> replaceM $ do
        pure . join_edges [(ill,ll),(lseq,iseq),(rseq,bseq){-,(rr,irr)-}] . replace [r,right_node] [] $ graph
      (Dup _ _ _, _) -> expand rr Down
      (_, Dup _ _ _) -> expand ll Down
      (PiR (Rule cs (Sequent prseq pbg (pr, pbd))),
       PiL t (Rule (prr, pll) (Sequent iseq il ()))) | rr == pr && il == ll -> replaceM $ do
        let Just (Sequent (ptg, mrr) (mll, ptd)) = lookup t cs
        seqAs <- assert (length prr == length mll) $ replicateM (length mll) (mkEID "seqA")
        seqBs <- assert (length pll == length mrr) $ replicateM (length mrr) (mkEID "seqB")
        let mkcutA (Sequent rs () r) ml ls bs = Cut (Rule (Sequent rs () r, Sequent ls l ()) (Sequent bs () ()))
        let newcuts = zipWith mkcutA prr mll ++ zipWith Cut mrr pll
        pure . join_edges (zip ptg pbg ++ zip ptd pbd) .
          replace [r,right_node,left_node] newcuts $ graph
      (Bang (Rule (Sequent oseq ol (o, or)) (Sequent iseq il (irr, ir))), BangC (Rule (Sequent ocseq f ()) (Sequent icseq ill ()))) | rr == irr && ll == ill -> replaceM $ do
        let repF = replicateM (length f)
        cut_bang <- repF mkedge
        bang_dup_main <- repF mkedge
        bang_dup_aux_r <- repF $ replicateM (length or) mkedge
        bang_dup_aux_l <- repF $ replicateM (length ol) mkedge
        cw_bang_l <- repF $ replicateM (length il) mkedge
        cw_bang_r <- repF $ replicateM (length ir) mkedge
        let tr = transpose
        let cuts = zipWith Cut cut_bang f
        let dup = if f == [] then [] else return . Dup $ [(R,o,bang_dup_main)] ++ zipWith (R,,) or (tr bang_dup_aux_r) ++ zipWith (L,,) ol (tr bang_dup_aux_l)
        let cws_l = zipWith BangCW il (tr cw_bang_l)
        let cws_r = zipWith WhimCW ir (tr cw_bang_r)
        let bangs = zipWith4 Bang cut_bang (zipWith Ctx cw_bang_l cw_bang_r) bang_dup_main (zipWith Ctx bang_dup_aux_l bang_dup_aux_r)
        return ([r,right_node,left_node], dup ++ bangs ++ cuts ++ cws_l ++ cws_r)
      (Bang (Rule (Sequent oseq ol (o, or)) (Sequent iseq il (irr, ir))), BangD (Rule (Sequent doseq f ()) (Sequent dseq ill ()))) | ill == ll -> let
        (del,add) = join_edges (il++or) (ol++or)
        cut = Cut o f
        in replace ([r,left_node,right_node]++del) ([cut]++add)
      (_, Bang (Rule (Sequent oseq ol (o, or)) (Sequent iseq il (irr, ir)))) | Just idx <- ll `elemIndex` il -> let
        newll = ol !! idx
        cut = Cut rr newll
        bang = Bang i (Ctx (delete idx il) ir) o (Ctx (delete idx ol) or)
        in replace [r,left_node] [cut,bang]
      (_,_) -> replaceM $ traceShow (r,right_node,left_node) undefined
  (Identity (Rule () (Sequent _ ll _)),Right,Down) -> expand ll Up
  (Identity (Rule () (Sequent _ _ rr)),Left,Down) -> expand rr Up
  (p,_,Up) -> expand i Up
    where
      i = case p of
          (PiR (Rule _ (Sequent _ _ (i, _)))) -> i
          (PiL _ (Rule _ (Sequent _ i _))) -> i
          (SigmaR _ (Rule _ (Sequent _ _ i))) -> i
          (SigmaL (Rule _  (Sequent _ (_, i) _))) -> i
          (Bang (Rule _ (Sequent _ _ ((_,i), _)))) -> i
        -- todo: handle auxiliary ports. Although we will never encounter them during reduction?
          (BangD (Rule _ (Sequent _ (_,i) ()))) -> i
          (BangC (Rule _ (Sequent _ i ()))) -> i
          (BangW (Rule _  (Sequent _ i ()))) -> i
          (Whim (Rule _  (Sequent _ (_, (_,i)) _))) -> i
          (WhimD (Rule _ (Sequent _ () (_,i)))) -> i
          (WhimC (Rule _ (Sequent _ () i))) -> i
          (WhimW (Rule _ (Sequent _ () i))) -> i
