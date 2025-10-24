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
