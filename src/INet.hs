{-# LANGUAGE DataKinds, KindSignatures, UnicodeSyntax, RecordWildCards,
DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

import Data.IORefStable
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad
import Data.List (isPrefixOf)

data Port = Eraser | Port (IORefStable Edge)
  deriving (Eq,Ord)
instance Show Port where
  show _ = "Port"

data Edge = Edge Node Direction Node Direction -- assert: I then O
   deriving (Eq,Ord,Show)

data NodeP p
  = Initiator {out ∷ p}
  | FanIn  {name :: String, level ∷ Int, out ∷ p, ins ∷ [(p,Int)]} -- (port, level)
  | FanOut {name :: String, level ∷ Int, inp ∷ p, outs ∷ [(p,Int)]}
  | Applicator {inp, func, arg ∷ p}
  | Abstractor {inp, bind, body ∷ p}
  | Constant {name ∷ String, inp, out :: p}
  | Case {names ∷ [String], inp, out ∷ p, alts ∷ [p]}
   deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

type Node = NodeP Port

data Direction
   = Inp | Func | Arg | Body | Bind | Out | Ins Int | Alts Int | Outs Int
   deriving (Eq,Ord,Show)

data D = I | O
   deriving (Eq,Ord,Show)

getD :: Direction -> D
getD Inp = I
getD Func = O
getD Arg = O
getD Body = O
getD Bind = I
getD Out = O
getD (Ins _) = I
getD (Alts _) = O
getD (Outs _) = O

ports node = case node of
  Initiator  {..} → [Out]
  Applicator {..} → [Func,Inp,Arg]
  Abstractor {..} → [Inp,Body,Bind]
  Constant   {..} → [Inp,Out]
  FanIn      {ins = is, ..} → [Out]++(map (\(i,_) -> Ins i) $ zip [0..] is)
  FanOut     {outs = os,..} → [Inp]++(map (\(i,_) -> Outs i) $ zip [0..] os)
  Case       {alts=as,..} → [Inp,Out] ++ (map (\(i,_) -> Alts i) $ zip [0..] as)

primaryPort a = ports a !! 0

port :: Direction -> Node -> Port
port Inp = inp
port Func = func
port Arg = arg
port Body = body
port Bind = bind
port Out = out
port (Ins i) = fst . (!! i) . ins
port (Outs i) = fst . (!! i) . outs
port (Alts i) = (!! i) . alts

getPort :: Node -> Direction -> IORefStable Edge
getPort n d | Port p <- port d n = p

link :: Node -> Direction -> Node -> Direction -> IO ()
link a ad b bd | getD ad /= I || getD bd /= O = error "bad direction"
link a ad b bd = do
  let e = Edge a ad b bd
  writeIORefStable (getPort a ad) e
  writeIORefStable (getPort b bd) e

--BOHM: f.nform[i].nform[f.nport[i]] == f
--mine: (a,ad) = follow(f, i); f == follow a ad
follow source dir = do
  Edge a ad b bd <- readIORefStable (getPort source dir)
  return $ case getD dir of
    I -> (b,bd)
    O -> (a,ad)

relink :: Node -> Direction -> Bool -> Node -> Direction -> Bool -> IO ()
relink a ad True b bd bf = do
  (a', ad') <- follow a ad
  relink a' ad' False b bd bf
relink a ad af b bd True = do
  (b', bd') <- follow b bd
  relink a ad af b' bd' False
relink a ad False b bd False =
  link a ad b bd

getPorts :: Node -> Direction -> [Direction]
getPorts a ppa = filter (/= ppa) $ ports a

getNodes :: Node -> IO (Set Node, Set Edge)
getNodes n = process Set.empty Set.empty [n]
  where
    process :: Set Node -> Set Edge -> [Node] -> IO (Set Node, Set Edge)
    process nodes edges [] = return (nodes, edges)
    process nodes edges (n:ns) | Set.member n nodes = process nodes edges ns
    process nodes edges (n:ns) = do
      let dirs = ports n
      es <- mapM (readIORefStable . getPort n) dirs
      ps <- mapM (fmap fst . follow n) dirs
      process (Set.insert n nodes) (Set.fromList es `Set.union` edges) (ps++ns)

-- zip [0..] . toList


clone node = traverse mkRef node
  where
    mkRef Eraser = return Eraser
    mkRef (Port _) = do
      p <- newIORefStable undefined
      return $ Port p

data State = State {
  root :: Node,
  stack :: [Node]
} deriving (Eq,Ord,Show)

lo_redex t = do
  let d = primaryPort t
  case getD d of
    I -> return [t]
    O -> do
      (u,_) <- follow t d
      r <- lo_redex u
      return $ r ++ [t]

initStep :: Node -> IO State
initStep root = do
  stack <- lo_redex root
  return $ State {..}

rewriteStep :: State -> IO State
rewriteStep (State {..}) = do
  let t:s:u:stackrest = stack
  reducePair t (primaryPort t) s (primaryPort s)
  lopair <- lo_redex u
  return $ State { stack = lopair ++ stackrest,..}

updateLevel you me pb = case me of
    FanIn {} -> maybeLevelUp
    FanOut {} -> maybeLevelUp
    _ → me
  where
    maybeLevelUp =
     case you of
      FanIn {} → me {level = level me + 1}
      FanOut {} → me {level = level me + 1}
      _ → me

makenodes a pa b pb ppb =
  forM pb $ \p -> do
    x' <- clone a
    let x = updateLevel b x'
    return $ case getD p == getD ppb of
      False -> x
      True -> case x of
        FanIn {..} -> FanOut {inp = out, outs=ins, ..}
        FanOut {..} -> FanIn {out = inp, ins=outs, ..}

makeOuterLinks a pa b_new ppb =
  forM_ pa $ \(pi,p) ->
    relink a p True (b_new !! pi) ppb False (getD p == O)

commute ∷ Node -> Direction -> Node -> Direction -> IO ()
commute a ppa b ppb = do
  let pa = getPorts a ppa
  let pb = getPorts b ppb
  a_new <- makenodes a pa b pb ppb
  b_new <- makenodes b pb b pb ppb
  makeOuterLinks a' (zip [0..] pa) b_new ppb
  makeOuterLinks b' (zip [0..] pb) a_new ppa
  forM_ (zip [0..] pb) $ \(pbi,pb1) ->
    forM_ (zip [0..] pa) $ \(pai,pa1) ->
      link (a_new !! pbi) pa1 (b_new !! pai) pb1 (getD pa1 /= O)

eqNode :: Node -> Node -> Bool
eqNode (FanIn {level = a, ins = as}) (FanOut {level = b, outs=bs}) = a == b && length as == length bs
eqNode _ _ = False

reducePair a ppa b ppb = case () of
  () | eqNode a b && ppa == ppb ->
    forM_ (getPorts a ppa) $ \p -> do
      relink a p True b p True False -- nothing that annihilates changes directions
  () | Applicator {} <- a, Abstractor {} <- b, ppa == Func, ppb == Inp -> do
    relink a Inp True b Body True False
    relink b Bind True a Arg True False
  _ -> commute a ppa b ppb

fan  fan-like


f1.index < f2.index
  new1 = clone f2, index = f2->index+f1->nlevel[2];
  f2->index = f2->index+f1->nlevel[1];
  new2 = clone f1

{-
reduce Fan Fan | index==index = do
  assert nlevels == nlevels
  relink a p1 True b p1 True False
  relink a p2 True b p2 True False
reduce Triangle Triangle | index == index
app lambda
app lambdaunb = connect app Inp l Body
                connect Erase ap Arg
                clean_garbage

      fan_uns1 = Fan w/ unshared port
reduce_redex(f1,f2)
FORM  f1,f2    *new1,  *new2;
	clean();

     else{ /* f1.index < f2.index */
	switch (f1->name)
	{
	   case FAN:
	      if (f2->name != TRIANGLE) fan_int++;
	      if (f2->name == LAMBDA)
			f1->num_safe=false;
	      switch (f2->name)
	      {
		 case APP:
		 case LAMBDA:
		    allocate_form(&new1,f2->name,f2->index+f1->nlevel[2]);
		    f2->index = f2->index+f1->nlevel[1];
		    allocate_form(&new2,FAN,f1->index);
		    new2->num_safe=f1->num_safe;
		    new2->nlevel[1] = f1->nlevel[1];
		    new2->nlevel[2] = f1->nlevel[2];
		    connect1(f2, 0, f1->nform[1], f1->nport[1]);
		    connect1(new1, 0, f1->nform[2], f1->nport[2]);
		    connect1(new2, 0, f2->nform[1], f2->nport[1]);
		    connect1(f1, 0, f2->nform[2], f2->nport[2]);
		    connect(f1,1,f2,2);
		    connect(f1,2,new1,2);
		    connect(f2,1,new2,1);
		    connect(new1,1,new2,2);
		    break;

		 case FAN:
		    allocate_form(&new1,f2->name,f2->index+f1->nlevel[2]);
		    f2->index = f2->index+f1->nlevel[1];
		    new1->num_safe=f2->num_safe;
		    new1->nlevel[1] = f2->nlevel[1];
		    new1->nlevel[2] = f2->nlevel[2];
		    allocate_form(&new2,FAN,f1->index);
		    new2->num_safe=f1->num_safe;
		    new2->nlevel[1] = f1->nlevel[1];
		    new2->nlevel[2] = f1->nlevel[2];
		    connect1(f2, 0, f1->nform[1], f1->nport[1]);
		    connect1(new1, 0, f1->nform[2], f1->nport[2]);
		    connect1(new2, 0, f2->nform[1], f2->nport[1]);
		    connect1(f1, 0, f2->nform[2], f2->nport[2]);
		    connect(f1,1,f2,2);
		    connect(f1,2,new1,2);
		    connect(f2,1,new2,1);
		    connect(new1,1,new2,2);
		    break;

		 case TRIANGLE:
		    allocate_form(&new1,f2->name,f2->index+f1->nlevel[2]);
		    new1->num_safe=f2->num_safe;
		    new1->nlevel[1] = f2->nlevel[1];
		    f2->index = f2->index+f1->nlevel[1];
		    connect1(f2, 0, f1->nform[1], f1->nport[1]);
		    connect1(new1 , 0, f1->nform[2], f1->nport[2]);
		    connect1(f1, 0, f2->nform[1], f2->nport[1]);
		    connect(f1,1,f2,1);
		    connect(f1,2,new1,1);
		    break;

  case TRIANGLE:
	      if (f2->name == LAMBDA)
		 f1->num_safe = false;
	      switch (f2->name)
		 {
		    case APP:
		    case LAMBDA:
		    case FAN:
		       allocate_form(&new1,f1->name,f1->index);
		       new1->nlevel[1] = f1->nlevel[1];
		       new1->num_safe=f1->num_safe;

		       f2->index = f2->index + f1->nlevel[1];

		       connect1(f2, 0, f1->nform[1], f1->nport[1]);
		       connect1(new1, 0, f2->nform[1], f2->nport[1]);
		       connect1(f1, 0, f2->nform[2], f2->nport[2]);
		       connect(f1,1,f2,2);
		       connect(new1,1,f2,1);
		       break;

		    case TRIANGLE:
		    case LAMBDAUNB:
		    case UNS_FAN1:
		    case UNS_FAN2:
		       f2->index = f2->index + f1->nlevel[1];

		       connect1(f2, 0, f1->nform[1], f1->nport[1]);
		       connect1(f1, 0, f2->nform[1], f2->nport[1]);
		       connect(f1,1,f2,1);
		       break;

		 }
	   break;
	}
     }
}

-}

type Director = Int
data Block = Block Int [Director]
  deriving (Eq,Ord)
data Level = Level Block [Level]
  deriving (Eq,Ord)
data Stack = Stack Int [Level]
  deriving (Eq,Ord)

showD i = "LRABCDEFGHIJK" !! i

instance Show Block where
  show (Block i []) | i < 10 = show i
  show (Block i ds) = "(" ++ show i ++ map showD ds ++ ")"

instance Show Level where
  show (Level b []) = show b
  show (Level b ls) = "[" ++ show b ++ concatMap show ls ++ "]"

instance Show Stack where
  show (Stack i l) = show i ++ concatMap show l

isStackExtension (Stack _ i) (Stack _ j) = s_ext i j
  where
    s_ext i [] = True
    s_ext [] j = False
    s_ext (i:is) (j:js) = level_ext i j && s_ext is js

    level_ext (Level a as) (Level b bs) = b_ext a b && l_ext as bs

    l_ext i [] = True
    l_ext [] j = False
    l_ext (i:is) (j:js) = level_ext i j && l_ext is js

    b_ext (Block i ds) (Block j es) = i == j && isPrefixOf ds es


data Lambda = App Lambda Lambda | Abs Int Lambda
{-
readback :: Node -> Direction -> Stack -> Lambda
readback source dir stack = do
  (term, from_dir) <- follow source dir
  case term of
    Initiator {} -> error "readback of initiator"
    Applicator {} | from_dir == Inp -> do
      f <- readback term Func stack
      t <- readback term Arg stack
      return $ App f t
    Abstractor {} | from_dir == Inp -> do

      return $ Abs
    DelimiterO {}
    DelimiterC {}
    FanIn {}
    FanOut {}
    Constant {}
    Case {}
-}
{-
    case "operator": {
      if (from_dir == "inp") {
        let t = { tag: "operator", cases: [] };
        stack.lambdas = [{ term, stack }].concat(stack.lambdas);
        for (let x = 0; x < term.arities.length; x++) {
          assert(term.names[x]);
          t.cases.push([term.names[x], term.arities[x], readback(term, x, stack)]);
        }
        return t;
      }
      if (from_dir >= term.arities.length) {
        let i = from_dir - term.arities.length;
        let j;
        for (j = 0; j < term.arities.length; j++) {
          if (i < term.arities[j]) break;
          i -= term.arities[j];
        }
        let b = 0;
        while (term !== stack.lambdas[b].term) b++;
        if (!isStackExtension(stack.lambdas[b].stack, stack)) {
          assert(false);
          isStackExtension(stack.lambdas[b].stack, stack)
        }
        return {
          tag: "symbol", idx: b, rule: term.names[j], rule_idx: i
        };
      }
      assert(false);
    }
    case "multiplexer": {
      let e = term.labelEnv, i = e.id;
      let levels = Array.from(stack.levels); stack.levels = levels;
      if (from_dir == "val") {
        // σ[(jLδ)l]_i -> σ[jδl]_i
        let d;
        assert(levels[i] && levels[i].directors);
        assert(levels[i].directors.length == 1);
        let l = clone(levels[i]);
        d = l.directors[0];
        l.directors = l.directors.slice(1);
        levels[i] = l;
        assert(d.name == term.name);
        return readback(term, d.index, stack);
      } else {
        // σ[jδl]_i -> σ[(jLδ)l]_i
        let l;
        if (!levels[i]) {
          l = Level(e);
        } else {
          l = clone(levels[i]);
          assert(l.env == e);
          if (l.directors.length == 1) {
            assert(l.directors[0].index == from_dir);
            assert(l.directors[0].name == term.name);
            l.directors = [];
          } else assert(l.directors.length == 0);
        }
        l.directors = [Director(e, term.name, from_dir)].concat(l.directors);
        levels[i] = l;
        return readback(term, "val", stack);
      }
    }
    case "constant": {
      let t = { tag: "tuple", name: term.name, terms: [] };
      for (let i = 0; i < term.length; i++) {
        t.terms.push(readback(term, i, stack));
      }
      return t;
    }
    case "case": {
      if (from_dir == "inp") {
        let c = { tag: "case", cases: [], exp: readback(term, "out", stack) };
        for (let i = 0; i < term.arities.length; i++) {
          c.cases.push([term.names[i], readback(term, i, stack)]);
        }
        return c;
      }
    }
  }
  assert(false);
}

compileShare ∷ (View [Port] n, View NodeLS n) ⇒ Rule n
compileShare = do
  Multiplexer {out = o, ins = is} ← node
  case is of
    [ ] → replace $ byNode Eraser {inp = o}
    [i] → rewire [[o,i]]
    ins → let (ins1, ins2) = splitAt (length ins `div` 2) ins in replace $ do
      (o1,o2) ← (,) <$> byEdge <*> byEdge
      byNode $ Duplicator {level = 0, inp = o, out1 = o1, out2 = o2}
      byNode $ Multiplexer {out = o1, ins = ins1}
      byNode $ Multiplexer {out = o2, ins = ins2}

withoutIdx ∷ [a] → Int → [a]
withoutIdx xs i = let (ys,zs) = splitAt i xs in ys ⧺ tail zs

insertIdx ∷ Int → a → [a] → [a]
insertIdx i x xs = let (l,r) = splitAt i xs in l ⧺ [x] ⧺ r

split ∷ Int → Int → [a] → [[a]]
split i n [] = replicate n []
split i n xs = let (x,xs') = splitAt i xs in x : split i n xs'

transpose' n [] = replicate n []
transpose' n xs = transpose xs

annihilateDelimiters ∷ (View [Port] n, View NodeLS n) ⇒ Rule n
annihilateDelimiters = do
  rewrite ← annihilate
  Delimiter {} ← liftReader . inspectNode =<< previous
  return rewrite

-- This rule doesn't trigger for constants with arguments
eliminateDelimiterConstant ∷ (View [Port] n, View NodeLS n) ⇒ Rule n
eliminateDelimiterConstant = do
  c@Constant {args = as, name = n} :-: Delimiter {inp = iD} ← activePair
  require (inp c ≢ iD && as == [])
  replace $ byNode $ Constant {inp = iD, args = [], name = n}

eliminateDelimiterEraser ∷ (View [Port] n, View NodeLS n) ⇒ Rule n
eliminateDelimiterEraser = do
  c@Eraser {} :-: Delimiter {inp = iD} ← activePair
  require (inp c ≢ iD)
  replace $ byNode $ Eraser {inp = iD}

eliminateDuplicator ∷ (View [Port] n, View NodeLS n) ⇒ Rule n
eliminateDuplicator = do
  Eraser {inp = iE} ← node
  Duplicator {inp = iD, out1 = o1, out2 = o2} ← neighbour =<< previous
  require (iE ≡ o1 ∨ iE ≡ o2)
  if iE ≡ o1
    then rewire [[iD,o2]]
    else rewire [[iD,o1]]

beta ∷ (View [Port] n, View NodeLS n) ⇒ Rule n
beta = do
  Applicator {inp = ai, func = f, arg = a} :-: Abstractor {body = b, var = v} ← activePair
  replace $ do
    byNode $ Delimiter {level = 0, inp = ai, out = b}
    byNode $ Delimiter {level = 0, inp = a, out = v}

commuteDelimiter ∷ (View [Port] n, View NodeLS n) ⇒ Rule n
commuteDelimiter = do
  rewrite ← commute
  Delimiter {} ← liftReader . inspectNode =<< previous
  return rewrite

applyConstant ∷ (View [Port] n, View NodeLS n) ⇒ Rule n
applyConstant = do
  Applicator {inp = i, arg = a} :-: Constant {name = n, args = as} ← activePair
  replace $ byNode $ Constant {inp = i, name = n, args = as ++ [a]}

applyOperator ∷ (View [Port] n, View NodeLS n) ⇒ Rule n
applyOperator = do
  Applicator {inp = i, arg = a} :-: Operator {ops = os, arity = ar, lmop = l, function = fn, name = n} ← activePair
  require (ar > length os)
  replace $ byNode $ Operator {inp = i, ops = os ++ [a], arity = ar, lmop = l, function = fn, name = n}

-- TODO: Require that the lmoPort is not on one of the unreduced ports yet
-- Do we only reduce operator args, if the operator has all args already?
reduceOperand ∷ (View [Port] n, View NodeLS n) ⇒ Rule n
reduceOperand = do
  o@(Operator {ops = os, lmop = lmo}) ← node
  opid ← previous
  let ports = inspect o ∷ [Port]
  let lmoport = ports !! lmo
  -- only change the lmo port if it is on top or if it is attached to a Constant
  require (lmo == 0) <|> do {Constant {} ← nodeWith lmoport; return ()}
  port ← branch os -- get a pattern that matches each port in os
  -- we require that at least one node attached to the operator is not a constant
  requireFailure $ do
    Constant {} ← nodeWith port
    return ()
  -- we need to add one, since the input port is not part of os, but is part of the port numbering
  let unreducedport = 1 + fromJust (elemIndex port os)
  return $ updateNode opid (o {lmop = unreducedport})

execOperator ∷ forall n. (View [Port] n, View NodeLS n) ⇒ Rule n
execOperator = do
  Operator {inp = i, ops = os, arity = ar, function = fn, name = n} ← node
  opid ← previous
  require (length os == ar)
  -- check that all args are constants
  argss ← forM os $ \o → do
      c@Constant {} ← adverse o opid
      return c
  case fn (map name argss) of
    Nothing → mempty
    Just n' → replace $ byNode $ Constant {inp = i, args = [], name = n'}

caseNode ∷ (View [Port] n, View NodeLS n) ⇒ Rule n
caseNode = do
  -- the order of constant and case here is important, otherwise strategies don't work
  Case {inp = i, alts = alts, names = names} :-: Constant {name = n, args = as} ← activePair
  let matchingport = alts !! (fromJust $ elemIndex n names)
  let nn = length alts
  let m = length as
  -- We generate m new applicator nodes with m+1 new edges connecting them
  if m > 0 then
    replace $ do
      es ← replicateM (m+1) byEdge
      byWire matchingport (es !! 0) -- We merge the first new edge with the matching port from the Case node
      byWire i (es !! m) -- We merge the last new edge with the input edge of the Case node
      mconcat [byNode $ Applicator {inp = es !! (i+1), func = es !! i, arg = as !! i} | i ← [0..m-1]]
      mconcat [byNode $ Eraser {inp = alts !! i} | i ← filter (/= fromJust (elemIndex n names)) [0..nn-1]]
       else do
    replace $ do
      byWire i matchingport -- Attach the alternative directly to the input of the case node
      mconcat [byNode $ Eraser {inp = alts !! i} | i ← filter (/= fromJust (elemIndex n names)) [0..nn-1]]

-- | Not the readback semantics as defined in the paper. Just a non-semantics-preserving erasure of all
-- delimiters to make the graph more readable
readback ∷ (View [Port] n, View NodeLS n) ⇒ Rule n
readback = do
  Delimiter {inp = i, out = o} ← node
  rewire [[i,o]]

-}
