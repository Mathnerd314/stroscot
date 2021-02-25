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
import Data.Traversable(traverse)
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
    "digraph {\nrankdir=\"BT\"\n" ++ nodetxt ++ edgetxt ++ "}\n"

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

class FmapEID s where
  fmapEID :: (EID -> EID) -> s -> s
instance FmapEID Syntax where
  fmapEID f s = case s of
    PiR r -> PiR (fmapEID f r)
    PiL t r -> PiL t (fmapEID f r)
    SigmaR t r -> SigmaR t (fmapEID f r)
    SigmaL r -> SigmaL (fmapEID f r)
    Bang r -> Bang (fmapEID f r)
    BangD r -> BangD (fmapEID f r)
    BangC r -> BangC (fmapEID f r)
    BangW r -> BangW (fmapEID f r)
    Whim r -> Whim (fmapEID f r)
    WhimD r -> WhimD (fmapEID f r)
    WhimC r -> WhimC (fmapEID f r)
    WhimW r -> WhimW (fmapEID f r)
    Identity r -> Identity (fmapEID f r)
    Cut r -> Cut (fmapEID f r)
    Use t r -> Use t (fmapEID f r)
    Assign t r -> Assign t (fmapEID f r)
instance (FmapEID a, FmapEID b) => FmapEID (Rule a b) where
  fmapEID f (Rule t b) = Rule (fmapEID f t) (fmapEID f b)
instance (FmapEID a, FmapEID b) => FmapEID (Sequent a b) where
  fmapEID f (Sequent{..}) = Sequent
    { turnstile = fmapEID f turnstile
    , left = fmapEID f left
    , right = fmapEID f right
    }
instance (FmapEID a, FmapEID b) => FmapEID (a, b) where
  fmapEID f (a,b) = (fmapEID f a, fmapEID f b)
instance (FmapEID a) => FmapEID [a] where
  fmapEID f xs = map (fmapEID f) xs
instance FmapEID () where
  fmapEID f () = ()
instance FmapEID Tag where
  fmapEID f x = x
instance FmapEID EID where
  fmapEID f e = f e
instance FmapEID Level where
  fmapEID f l = l

reduce :: Graph -> Edges -> Edge -> GDir -> ([(Edge,Syntax)], State Int Graph)
reduce graph edges e@(from_id,to_id,einf) dir = let
  r = findNode graph e dir
  s = e_side einf

  follow e = findNode graph (findEdge edges e)

  replaceM :: State Int ([Syntax], [Syntax]) -> ([(Edge,Syntax)], State Int Graph)
  replaceM s = ([(e,r)], do
    (del, add) <- s
    return $ filter (\x -> not (x `elem` del)) graph ++ add
    )

  replace :: [Syntax] -> [Syntax] -> ([(Edge,Syntax)], State Int Graph)
  replace del add = replaceM (return (del,add))

  join_edges :: [EID] -> [EID] -> ([Syntax],[Syntax])
  join_edges top bottom = let
    bot_nodes :: [Syntax]
    bot_nodes = map (\e -> follow e Down) bottom
    change :: EID -> EID -> EID -> EID
    change bn bo b = if b == bo then bn else b
    change_all :: EID -> EID -> Syntax -> Syntax
    change_all bn bo rr = fmapEID (change bn bo) rr
    new_bots = zipWith3 change_all top bottom bot_nodes
    in (bot_nodes,new_bots)

  expand en dn = let (rest, g) = reduce graph edges (findEdge edges en) dn in ((e,r) : rest, g)
  in case (r,s,dir) of
  (Identity (Rule () (Sequent _ ll _)),Right,Down) -> expand ll Up
  (Identity (Rule () (Sequent _ _ rr)),Left,Down) -> expand rr Up
  (p,_,Up) | tagName p /= "Cut" -> expand i Up
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
 {-
  -- active cases
  (Dup xs, _, _) -> let
    didx = fromJust $ (case dir of Down -> in_num; Up -> out_num) gd
    (side,dup_out,dup_ins) = xs !! didx
    dup_target = follow dup_out Down
    repD = replicateM (length dup_ins)
    in case dup_target of
      Identity _ _ | dir == Down -> expand dup_out Down
      Dup _ | dir == Down -> expand dup_out Down
      Identity ll rr | dir == Up -> let
        id_edge@(_,_,_,gdi) = findEdge edges (case side of R -> ll; L -> rr)
        other_node = findNode graph id_edge Up
        didx2 = assert (other_node == r) $ fromJust (out_num gdi)
        dup_cleaned = Dup $ deleteM (sort [didx,didx2]) xs
        (_,_,other_ins) = xs !! didx2
        (dup_l,dup_r) = case assert (side == s) s of L -> (dup_ins,other_ins); R -> (other_ins, dup_ins)
        in replace [r,dup_target] (zipWith Identity dup_l dup_r++[dup_cleaned])
      PiRight i (Ctx [] []) cs | i == dup_out -> replaceM $ do
        let in_ctxs = trace "d_lam" $ replicate (length dup_ins) (Ctx [] [] :: Ctx String)
        (cases_rep, newdups) <- fmap unzip . flip mapM cs $ \(t,Ctx vl vr,Ctx [] []) -> do
          new_l <- repD $ replicateM (length vl) mkedge
          new_r <- repD $ replicateM (length vr) mkedge
          let dups = zipWith (L,,) vl (transpose new_l) ++ zipWith (R,,) vr (transpose new_r)
          let ctxs = zipWith Ctx new_l new_r
          let cases  = map (t,,Ctx [] []) ctxs
          return (cases, dups)
        let pirs = zipWith3 PiRight dup_ins in_ctxs (transpose cases_rep)
        let dup = Dup $ delete didx xs ++ concat newdups
        return ([r,dup_target],pirs++[dup])
      PiLeft _ t rr ll -> replaceM $ do
        new_l <- trace "d_app" $ repD $ replicateM (length ll) mkedge
        new_r <- repD $ replicateM (length rr) mkedge
        let newdups = zipWith (L,,) ll (transpose new_l) ++ zipWith (R,,) rr (transpose new_r)
        let ts = replicate (length dup_ins) t
        let pils = zipWith4 PiLeft dup_ins ts new_r new_l
        let dup = Dup $ delete didx xs ++ newdups
        return ([r,dup_target],pils++[dup])
      BangCW _ os -> replaceM $ do
        new_os <- repD $ replicateM (length os) mkedge
        let newdups = zipWith (L,,) os (transpose new_os)
        let newcws = zipWith BangCW dup_ins new_os
        let dup = Dup $ delete didx xs ++ newdups
        return ([r,dup_target],newcws++[dup])
      BangD _ o -> replaceM $ do
        os <- repD mkedge
        let newdup = (L,o,os)
        let bangds = zipWith BangD dup_ins os
        let dup = Dup $ delete didx xs ++ [newdup]
        return ([r,dup_target],bangds++[dup])
      Bang i (Ctx il ir) o (Ctx ol or) -> let
        this_dup = case dir of Down -> to_id; Up -> from_id
        check_input_edge i = let
          up_edge = findEdge edges i
          didx = fromJust . out_num . get_gd $ up_edge
          is_dup = get_from_id up_edge == this_dup
          in if is_dup then Right didx else Left up_edge
        dup_besides didxs = deleteM (sort didxs) xs
        get_ins = (\(_,_,i) -> i) . (xs !!)
        res = do
          idx <- check_input_edge i
          ildx <- mapM check_input_edge il
          irdx <- mapM check_input_edge ir
          return (get_ins idx, map get_ins ildx, map get_ins irdx, dup_besides ([idx]++ildx++irdx))
        in case res of
        Left e_next ->
            let (rest, g) = reduce graph edges e_next Up
            in ((e,r) : (e_next,dup_target) : rest, g)
        Right res@(id,ild,ird,dup_rest) -> replaceM $ do
          os <- repD mkedge
          ols <- repD $ replicateM (length ol) mkedge
          ors <- repD $ replicateM (length or) mkedge
          let newdups = [(R,o,os)] ++ zipWith (L,,) ol (transpose ols) ++ zipWith (R,,) or (transpose ors)
          let tr x = case x of [] -> replicate (length dup_ins) []; _ -> transpose x
          let ic = zipWith Ctx (tr ild) (tr ird)
          let oc = zipWith Ctx ols ors
          let bangs = zipWith4 Bang id ic os oc
          let dup = Dup $ dup_rest ++ newdups
          return ([r,dup_target],bangs++[dup])
      _ -> traceShow (r,dup_target) $ replace undefined undefined
  (Cut rr ll, _, Up)-> let
    right_node = follow rr Down
    left_node = follow ll Down
    in case (right_node,left_node) of
      (_, Identity ill irr) | ill == ll -> let
        (del,add) = join_edges [irr] [rr]
        in replace ([r,left_node]++del) add
      (Identity ill irr, _) | irr == rr -> let
        (del,add) = join_edges [ill] [ll]
        in replace ([r,right_node]++del) add
      (Dup _, _) -> expand rr Down
      -- (_, Dup _) -> expand ll Down
      (PiRight ir ic cs, PiLeft il t prr pll) | rr == ir && il == ll -> let
        [(_,Ctx mll mrr,context)] = filter (\(tr,_,_) -> tr == t) cs
        Ctx ic_l ic_r = ic
        Ctx context_l context_r = context
        (del,add) = trace "beta" $ join_edges (ic_l++ic_r) (context_l++context_r)
        in replace ([r,right_node,left_node]++del) (zipWith Cut prr mll ++ zipWith Cut mrr pll ++ add)
      (Bang irr (Ctx il ir) o (Ctx ol or), BangCW ill f) | rr == irr && ll == ill -> replaceM $ do
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
      (Bang _ (Ctx il ir) o (Ctx ol or), BangD _ f) -> let
        (del,add) = join_edges (il++or) (ol++or)
        cut = Cut o f
        in replace ([r,left_node,right_node]++del) ([cut]++add)
      (_, Bang i (Ctx il ir) o (Ctx ol or)) | Just idx <- ll `elemIndex` il -> let
        newll = ol !! idx
        cut = Cut rr newll
        bang = Bang i (Ctx (delete idx il) ir) o (Ctx (delete idx ol) or)
        in replace [r,left_node] [cut,bang]
      (_,_) -> traceShow (r,right_node,left_node) $ replace undefined undefined

-}