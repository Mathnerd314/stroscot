iterGraph edge_num graph ((redex,redex_dir):rs) = let
  names = nodeNames graph
  edgesG = edges graph
  redex_edge = findEdge edgesG redex
  (active, next) = reduce graph edgesG redex_edge redex_dir
  (next_graph, new_edge_num) = runState next edge_num
  in (graph, (edgesG, active)) : iterGraph new_edge_num next_graph rs

toDot edge_num graph rs = map (\(g,(e,a)) -> mkDot g e a) $ iterGraph edge_num graph rs

writeGraphs graph name limit = do
  -- let Root o = fromJust $ find ((=="Root") . tagName) graph
  let rs =
          replicate 5 ("rettop",Down) ++
          [("hl2i",Down)] ++
          replicate 2 ("hr",Down) ++
          [("x2",Up),("xj",Up),("xl2",Up)] ++
          -- ("y2",Up),("yj",Up),("yl2",Up),
          replicate 5 ("rettop",Down) ++
--          [("hl2p",Up),("hl2i",Up),("i",Up),("il",Up)] ++
          repeat ("rettop",Down)
  let out = zip [0..limit] (toDot 0 graph rs)
  forM_ out $ \(i,g) -> do
    putStrLn (show i)
    writeFile (name ++ "_" ++ show i ++ ".dot") g

main = writeGraphs example_m "example_m" 1000

findEdge edges en = fromJust $ find (\(n,_,_,_) -> en == n) edges

data GDir = Down | Up
   deriving (Eq,Ord,Show)

findNode graph e@(_,from,to,gd) Up = graph !! from
findNode graph e@(_,from,to,gd) Down = graph !! to

get_from_id (_,from,to,gd) = from
get_gd (_,_,_,gd) = gd

reduce :: Graph -> Edges -> Edge -> GDir -> ([(Edge,Rule)], State Int Graph)
reduce graph edges e@(_,from_id,to_id,gd) dir = let
  r = findNode graph e dir
  s = side gd
  expand en dn = let (rest, g) = reduce graph edges (findEdge edges en) dn in ((e,r) : rest, g)
  replaceM :: State Int ([Rule], [Rule]) -> ([(Edge,Rule)], State Int Graph)
  replaceM s = ([(e,r)], do
    (del, add) <- s
    return $ filter (\x -> not (x `elem` del)) graph ++ add
    )
  replace :: [Rule] -> [Rule] -> ([(Edge,Rule)], State Int Graph)
  replace del add = replaceM (return (del,add))
  follow e = findNode graph (findEdge edges e)
  join_edges :: [String] -> [String] -> ([Rule],[Rule])
  join_edges top bottom = let
    bot_nodes :: [Rule]
    bot_nodes = map (\e -> follow e Down) bottom
    change :: String -> String -> String -> String
    change bn bo b = if b == bo then bn else b
    change_all :: String -> String -> Rule -> Rule
    change_all bn bo rr = fmap (change bn bo) rr
    new_bots = zipWith3 change_all top bottom bot_nodes
    in (bot_nodes,new_bots)
  in case (r,s,dir) of
  (Identity ll _,R,Down) -> expand ll Up
  (Identity _ rr,L,Down) -> expand rr Up
  (PiLeft i _ _ _,_,Up) -> expand i Up
  (PiRight i _ _,_,Up) -> expand i Up
  (BangCW i _,_,Up) -> expand i Up
  (BangD i _,_,Up) -> expand i Up
  (Bang i _ _ _,_,_) -> expand i Up
  -- todo: handle auxiliary ports. Although we will never encounter them during reduction?
  (WhimCW i _,_,Up) -> expand i Up
  (WhimD i _,_,Up) -> expand i Up
  (Whim i _ _ _,_,Up) -> expand i Up
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

delete :: HasCallStack => Int -> [a] -> [a]
delete _ []     = error "delete: element not present"
delete 0 (y:ys) = ys
delete n (y:ys) = y : delete (n-1) ys

-- [Int] must be sorted ascending
deleteM :: HasCallStack => [Int] -> [a] -> [a]
deleteM [] ys = ys
deleteM [x] ys = delete x ys
deleteM (x:xs@(xp:_)) ys = assert (x < xp) $ delete x (deleteM xs ys)

