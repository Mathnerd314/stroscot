'use strict';
let multiplexer_parent_symbol = "⌧";
let explicit_lambda_delimiters = true;
let pop_scopes = true;
var stats = { annihilate: 0, commute: 0, beta: 0 }; window.stats = stats;

const assert = function (condition, message) {
  if (!condition) {
    debugger;
    console.log('Assert failed: ' + (message || ''));
  }
};

let clone_id = 0;
function clone(o) {
  let on = Object.assign({}, o);
  on.clone_id = clone_id++;
  return on;
}

let delimiter = (insideEnv, outsideEnv, full, reverse, inside, outside) => {
  return { tag: "delimiter", full, reverse, insideEnv, outsideEnv, inside, outside }
};
// in the initial graph inside is the parent, outside is the child.
// it faces up. so the graph looks like inside-]-outside.
// later lambda disintegrates to outside-[-inside and they eventually annihilate (hopefully)

let multiplexer = (name, labelEnv, ambientEnv, val) => { return { tag: "multiplexer", name, labelEnv, ambientEnv, val, length: 0 } };

let initiator = (out) => { return { tag: "initiator", out: out } };

// jumbo tuples
// (i, a, b, c)
let constant = (name, inp) => { return { tag: "constant", name, inp, length: 0 } };
// Paul Levy uses pm or "pattern match" as often there is only one pattern
// But Haskell uses case consistently. And I think it fits better since
// pattern-matching also implies patterns, whereas these are only tuples.
// case M of {i a b. N}
let cAse = (names, arities, inp, out) => { return { tag: "case", names, arities, inp, out, length: 0 } };

// A jumbo lambda-calculus function
// \{x. a, y. b}
let operator = (names, arities, inp) => {
  return { tag: "operator", names, arities, inp, length: 0 };
};
// Node for a @ <i, N>
let bigapplicator = (name, inp, func) => { return { tag: "bigapplicator", name, inp, func, length: 0 } };

let edge_id = 0;
let edge = (a, a_dir, b, b_dir) => { return { tag: "edge", a, a_dir, b, b_dir, active: false, id: edge_id++ } };

function ports(node) {
  let r;
  switch (node.tag) {
    case "initiator": return ["out"];
    case "applicator": return ["func", "inp", "arg"];
    case "abstractor": return ["inp", "body", "bind"];
    case "delimiter": return ["outside", "inside"];
    case "bigapplicator": r = ["func", "inp"]; break;
    case "constant": r = ["inp"]; break;
    case "multiplexer": r = ["val"]; break;
    case "case": r = ["out", "inp"]; break;
    case "operator": r = ["inp"]; break;
  }
  for (let i = 0; i < node.length; i++)
    r.push(i);
  return r;
}

let in_dirs = new Set(["inp", "bind", "inside"]);
let out_dirs = new Set(["func", "arg", "body", "out", "outside", "val"]);
function native_edge_dir(t, dir) {
  if (0 <= dir) {
    if (t.tag == "case" || t.tag == "operator") {
      return (dir >= t.arities.length) ? "in" : "out";
    }
    if (t.tag == "bigapplicator" || t.tag == "constant") {
      return "out";
    }
    if (t.tag == "multiplexer") {
      return "in";
    }
    assert(false);
  }

  if (in_dirs.has(dir)) {
    return "in";
  } else if (out_dirs.has(dir)) {
    return "out";
  }
  assert(false);
}
function edge_dir(t, dir) {
  let d = native_edge_dir(t, dir);
  return !t.reverse ? d : (d == "in" ? "out" : "in");
}

function primaryPort(node) {
  switch (node.tag) {
    case "initiator": return undefined;
    default:
      return ports(node)[0];
  }
}
let activePairs = [];
function link(a, a_dir, b, b_dir, reverse) {
  if (reverse)
    return link(b, b_dir, a, a_dir);

  if (b[b_dir] && b[b_dir].active) {
    b[b_dir].active = undefined;
    let i = activePairs.indexOf(b[b_dir]);
    assert(i >= 0);
    activePairs.splice(i, 1);
  }
  if (a[a_dir] && a[a_dir].active) {
    a[a_dir].active = undefined;
    let i = activePairs.indexOf(a[a_dir]);
    assert(i >= 0);
    activePairs.splice(i, 1);
  }
  let e = edge(a, a_dir, b, b_dir);
  b[b_dir] = a[a_dir] = e;
  if (primaryPort(a) == a_dir && primaryPort(b) == b_dir
    && !(a.tag == "delimiter" && b.tag == "delimiter" && a.full != "head" && b.full != "head")) {
    e.active = true;
    activePairs.push(e);
  }
  assert(edge_dir(a, a_dir) == "out");
  assert(edge_dir(b, b_dir) == "in");
}
function follow(source, dir) {
  let edge = source[dir], term, from_dir;
  if (source == edge.a && dir == edge.a_dir) {
    term = edge.b;
    from_dir = edge.b_dir;
  } else if (source == edge.b && dir == edge.b_dir) {
    term = edge.a;
    from_dir = edge.a_dir;
  } else {
    assert(false);
  }
  return [term, from_dir];
}

let n_envs = 0, all_envs = [];
function make_real_env() {
  let e = {
    id: n_envs++,
    multiplexers: new Set(),
    delimiters: new Set(),
    variant_envs: [], // id -> [env]
    is_variant_of: undefined
  };
  all_envs.push(e);
  return e;
}

let n_binding_envs = 0;
function make_env(parent) {
  let e = {
    n: n_binding_envs++,
    bindings: new Map(),
    parent,
    real_env: make_real_env()
  };
  return e;
}
function bind(env, v) {
  let name = v.name;
  assert(!env.bindings.has(name));
  env.bindings.set(name, v);
  v.labelEnv = env.real_env;
  v.ambientEnv = env.real_env;
  addEnv(v);
}
function addEnv(t) {
  if (t.tag == "multiplexer") {
    t.labelEnv.multiplexers.add(t);
    t.ambientEnv.multiplexers.add(t);
  } else if (t.tag == "delimiter") {
    t.insideEnv.delimiters.add(t);
    t.outsideEnv.delimiters.add(t);
  }
  // clear edges
  for (let p of ports(t)) {
    t[p] = null;
  }
}
function removeFromEnv(t) {
  if (t.tag == "multiplexer") {
    assert(t.labelEnv.multiplexers.has(t));
    assert(t.ambientEnv.multiplexers.has(t));
    t.labelEnv.multiplexers.delete(t);
    t.ambientEnv.multiplexers.delete(t);
  } else if (t.tag == "delimiter") {
    assert(t.insideEnv.delimiters.has(t));
    assert(t.outsideEnv.delimiters.has(t));
    t.insideEnv.delimiters.delete(t);
    t.outsideEnv.delimiters.delete(t);
  }
}

function compile(term) {
  let i = initiator();
  let e = make_env();
  resolve(e, term, i, "out");
  return i;
}

function resolve(env, term, parent, parent_dir) {
  let res = (term, parent, parent_dir) =>
    resolve(env, term, parent, parent_dir);
  if (typeof (term) == "string") {
    term = { tag: "symbol", name: term };
  }
  switch (term.tag) {
    case "app": {
      let t = {
        tag: "bigapp",
        name: "sole",
        func: term.func,
        terms: [term.arg]
      };
      return res(t, parent, parent_dir);
    }
    case "bigapp": {
      let a = bigapplicator(term.name);
      link(parent, parent_dir, a, "inp");
      res(term.func, a, "func");
      for (let t of term.terms) {
        res(t, a, a.length++);
      }
      return a;
    }
    case "tuple": {
      let c = constant(term.name);
      link(parent, parent_dir, c, "inp");
      for (let t of term.terms) {
        res(t, c, c.length++);
      }
    }
    case "abs": {
      let t = {
        tag: "operator",
        cases: [{ constr: "sole", vars: [term.name], rhs: term.body }]
      };
      return res(t, parent, parent_dir);
    }
    case "operator": {
      // semantically a lambda-case
      let o = operator([], []);
      o.arities.length = term.cases.length;
      link(parent, parent_dir, o, "inp");
      let bind_edges = [];
      for (let { constr, vars, rhs } of term.cases) {
        let new_env = make_env(env);
        o.names.push(constr);
        o.arities[o.length] = vars.length;
        for (let v_ of vars) {
          let v = multiplexer(v_, 0);
          bind(new_env, v);
          let d2 = delimiter(new_env.real_env, new_env.parent.real_env, "full");
          addEnv(d2);
          link(v, "val", d2, "inside");
          bind_edges.push(d2);
        }
        let d1 = delimiter(new_env.real_env, new_env.parent.real_env, "head", true);
        addEnv(d1);
        link(o, o.length++, d1, "outside");
        resolve(new_env, rhs, d1, "inside");
      }
      for (let d of bind_edges) {
        link(d, "outside", o, o.length++);
      }
      return o;
    }
    /*
    case "case": {
      let cs = cAse([], []);
      cs.arities.length = term.cases.length;
      if (!explicit_lambda_delimiters)
        cs.envs = [];
      link(parent, parent_dir, cs, "inp");
      res(term.exp, cs, "out") // compile the scrutinee
      let muxes = [];
      for (let { constr, vars, rhs } of term.cases) {
        cs.names.push(constr);
        cs.arities[cs.length] = vars.length;
        let new_env = make_env(env);
        for (let v_ of vars) {
          let v = multiplexer(v_, 0);
          muxes.push(v);
          bind(new_env, v);
        }
        resolve(new_env, rhs, cs, cs.length++);
      }
      for (let v of muxes) {
        link(v, "val", cs, cs.length++);
      }
      return cs;
    }
    */
    case "symbol": {
      let e = env, name = term.name;
      while (e && !(e.bindings.has(name))) {
        let d = delimiter(e.real_env, e.parent.real_env, "exit");
        link(parent, parent_dir, d, "inside");
        parent = d;
        parent_dir = "outside";
        e = e.parent;
      }
      assert(e);
      let v = e.bindings.get(name);
      link(parent, parent_dir, v, v.length++);
      return v;
    }
    case "let": {
      /*
      for (let [name, _rhs] of term.binds) {
        let v = multiplexer(name, 0);
        if (env.has(v.name)) {
          env.get(v.name).push(v);
        } else {
          env.set(v.name, [v]);
        }
      }

      for (let [name, rhs] of term.binds) {
        let x = env.get(name);
        let v = x[x.length - 1];
        resolve(rhs, v, "val");
      }

      // open scope for multiplexers
      let d = delimiter(0); d.reverse = true;
      link(parent, parent_dir, d, "outside");
      resolve(term.expr, d, "inside");

      for (let [name, _rhs] of term.binds) {
        env.get(name).pop();
      }
      return d;
      */
      // letrec is hard, for now just do simple syntactic lambda translation
      // let a = b in c -> (\a. c @ b)
      let body = term.expr;
      for (let [name, rhs] of term.binds) {
        body = {
          tag: "app",
          func: { tag: "abs", name, body: rhs },
          arg: body
        };
      }
      res(body, parent, parent_dir);
    }
  }
}

let colormap = ['black', '#a6cee3', '#1f78b4', '#b2df8a', '#33a02c', '#fb9a99', '#e31a1c',
  '#fdbf6f', '#ff7f00', '#cab2d6', '#6a3d9a', '#ffff99', '#b15928'];

let Graph = graphlibDot.graphlib.Graph;

function getdot(root) {
  // traverse
  let visitedEdges = new Set();
  let envs = new Set();

  var stack = [root];
  for (let e of activePairs) {
    stack.unshift(e.a);
    stack.unshift(e.b);
    visitedEdges.add(e);
  }
  let redexEdges = new Set();
  for (let [t, pp] of loStack) {
    let e = t[pp];
    stack.unshift(e.a);
    stack.unshift(e.b);
    visitedEdges.add(e);
    redexEdges.add(e);
  }

  let visitedNodes = new Set();
  while (stack.length > 0) {
    let node = stack.pop();
    if (!node || !node.tag)
      continue;
    if (visitedNodes.has(node))
      continue;
    visitedNodes.add(node);

    if (node.insideEnv)
      envs.add(node.insideEnv);
    if (node.outsideEnv)
      envs.add(node.outsideEnv);
    if (node.ambientEnv)
      envs.add(node.ambientEnv);
    if (node.labelEnv)
      envs.add(node.labelEnv);

    let names = ports(node);
    for (let n of names) {
      assert(n in node);
      assert(node[n]);
      let edge = node[n], term;
      if (node == edge.a && n == edge.a_dir) {
        term = edge.b;
        assert(edge.active !== undefined && term[edge.b_dir] == edge);
      } else {
        term = edge.a;
        assert(edge.active !== undefined && term[edge.a_dir] == edge);
      }
      stack.push(term);
      visitedEdges.add(edge);
    }
  }

  // Map of the things
  var num_nodes = 0;
  let m = new Map();
  for (let i of visitedNodes) {
    m.set(i, num_nodes++);
  }
  //  envs = Array.from(envs);
  //  envs.sort((a, b) => b.n - a.n);

  let g = new Graph({ multigraph: true });
  g.setGraph({});
  for (let c of visitedNodes) {
    let shape = (() => {
      switch (c.tag) {
        case "initiator": return "circle"; //"doublecircle";
        case "applicator": return "circle";
        case "abstractor": return "rect";
        //        case "constant": return "star";
        //        case "delimiter": return "house";
        //        case "multiplexer": return "fan";
        //        case "case": return "folder";
        //        case "operator": return "star";
        default: return "rect";
      }
    })();
    let human = (() => {
      let level = c.level ? `<sub>${c.level}</sub>` : "";
      let name = c.name && c.name !== "sole" ? `<sub>${c.name}</sub>` : "";
      switch (c.tag) {
        case "initiator": return `<span id=initiator>I</span>`;
        case "applicator": return `@`;
        case "bigapplicator": return `@${name}`;
        case "abstractor": return `λ${name}`;
        case "constant": return c.name;
        case "delimiter":
          let s; // ◜ 	◝ 	◞ 	◟
          if (c.full === "full") {
            s = c.reverse ? "ᙉ" : "ᙈ";
          } else if (c.full == "exit") {
            s = c.reverse ? "ᙁ" : "ᙀ";
          } else if (c.full == "head") {
            assert(c.reverse);
            s = "ᑍ";
          } else assert(false);
          let ss = c.reverse ? ["sub", "sup"] : ["sup", "sub"];
          let ids = [c.outsideEnv.id, c.insideEnv.id];
          if (c.reverse) ids = [ids[1], ids[0]];
          return `<${ss[0]}>${ids[0]}</${ss[0]}>${s}<${ss[1]}>${ids[1]}</${ss[1]}>`;
        case "multiplexer": return `<span class=${c.reverse ? "demux" : "mux"}>${c.name}</span>${level}`;
        case "case": return "case";
        case "operator":
          if (c.names.length == 1 && c.names[0] == "sole") {
            return `λ`;
          } else return `λ{${c.names}}`;
        default: assert(false);
      }
    })();
    let attrs = { shape, labelType: "html", label: human, padding: 0 };
    // if(c.clone_id) human += `<sup>${c.clone_id}</sup>`;
    if (c.ambientEnv && c.labelEnv) {
      let lr = [colormap[c.labelEnv.id], colormap[c.labelEnv.id]];
      let ud = ["transparent", colormap[c.ambientEnv.id]]; if (c.reverse) ud = [ud[1], ud[0]];
      attrs.labelStyle = "border-style: solid; border-width: 2px;"
      attrs.labelStyle += `border-top-color: ${ud[0]}; border-bottom-color: ${ud[1]}; border-left-color: ${lr[0]}; border-right-color: ${lr[1]};`;
    } else if (c.insideEnv && c.outsideEnv) {
      let lr = [colormap[c.insideEnv.id], colormap[c.outsideEnv.id]];
      let ud = lr; if (c.reverse) ud = [lr[1], lr[0]];
      attrs.labelStyle = "border-style: solid; border-width: 2px;"
      attrs.labelStyle += `border-top-color: ${ud[0]}; border-bottom-color: ${ud[1]}; border-left-color: ${lr[0]}; border-right-color: ${lr[1]};`;
    } else {
      attrs.style = "stroke: none;";
      attrs.padding = 1;
    }
    let i = m.get(c);
    g.setNode(i, attrs);
  }
  let arrows = colormap;
  for (let edge of visitedEdges) {
    let a = m.get(edge.a);
    let a_dir = ports(edge.a).indexOf(edge.a_dir);
    let b = m.get(edge.b);
    let b_dir = ports(edge.b).indexOf(edge.b_dir);
    if (a_dir > arrows.length) a_dir = 0;
    if (b_dir > arrows.length) b_dir = 0;
    let is_active = edge.active ? "stroke-width: 3; " : "";
    let is_spine = redexEdges.has(edge) || (activePairs[activePairs.length - 1] == edge) ? "stroke-dasharray: 2; " : "";
    g.setEdge(a, b, {
      label: edge.id,
      style: `${is_active}${is_spine}stroke: ${arrows[a_dir]}; fill: none;`,
      arrowheadStyle: `fill: ${arrows[b_dir]}`
    }, edge.id);
  }
  all_envs.forEach(e => {
    if (e.id == 0) return;
    g.setNode("e" + e.id, {
      shape: "rect",
      labelType: "html",
      label: "" + e.id,
      style: envs.has(e) ? `fill: ${colormap[e.id]}; stroke: gray;` : `fill: white; stroke: ${colormap[e.id]};`
    });
    for (let p of e.is_variant_of ? [e.is_variant_of] : []) {
      g.setEdge("e" + e.id, p.id == 0 ? 0 : "e" + p.id, { style: "stroke-dasharray: 2;" });
    }
  });
  return g;
}
function eqNode(n1, n2) {
  if (n1.tag !== n2.tag) return false;
  if (n1.tag === "delimiter" && n1.env === n2.env) {
    assert(n1.full == n2.full);
    return true;
  }
  if (n1.tag === "multiplexer" && n1.length === 0) assert(n1.level === 0 || isNaN(n1.level));
  if (n1.tag === "multiplexer" && n1.level === n2.level) {
    assert(n1.length == n2.length);
    assert(n1.name === n2.name);
    assert(n1.env === n2.env);
    assert(n1.originalEnv === n2.originalEnv);
    return true;
  }
  return false;
}
// BOHM: f.nform[i].nform[f.nport[i]] == f
// mine: [a,ad] = follow(f, i); f == follow(a,ad)

var loStack = [];
function lo_redex([t, pp]) {
  let u, ud, upp, tp;
  do {
    assert(t[pp].a == t);
    tp = t.tag == "initiator";
    loStack.push([t, pp]);
    [u, ud] = follow(t, pp);
    upp = primaryPort(u);
    [t, pp] = [u, upp];
  } while (tp || ud != upp);
}

function rewriteStepLo() {
  let [t, pp] = loStack.pop();
  let pair = t[pp];
  assert(pair.active || t.tag == "initiator");
  if (pair.active)
    reducePair(pair);
  lo_redex(loStack.pop());
}

function rewriteStep() {
  let pair = activePairs.pop();
  assert(pair.active); while (!pair.active) pair = activePairs.pop();
  reducePair(pair);
}

function relink(a, a_dir, af, b, b_dir, bf, reverse) {
  if (af)
    [a, a_dir] = follow(a, a_dir);
  if (bf)
    [b, b_dir] = follow(b, b_dir);
  if (reverse)
    [a, a_dir, b, b_dir] = [b, b_dir, a, a_dir];

  link(a, a_dir, b, b_dir);
}

// make a the parent of b
function reparentEnv(a, b) {
  let eo = b.env;
  for (let e of [eo, ...eo.variant_envs.flat()]) {
    assert(e.parents.size == 1);
    for (let p of e.parents) {
      p.children.delete(e);
      e.parents.delete(p);
    }
    assert(e.parents.size == 0);
    e.parents.add(a.env);
    e.parent = a.env;
    a.env.children.add(e);
  }
  if (eo.is_variant_of) {
    eo.v_diff_stack.unshift([a.env, a.full]);
  }
}

function reducePair(pair) {
  assert(pair.active); pair.active = false;
  let a = pair.a, b = pair.b, ppa = pair.a_dir, ppb = pair.b_dir;
  if (eqNode(a, b) && ppa == ppb) {
    // annihilate
    stats.annihilate++;
    for (let p of ports(a)) {
      if (p == ppa) continue;
      relink(a, p, true, b, p, true);
    }
    removeFromEnv(a);
    removeFromEnv(b);
    return;
  }
  if (a.tag == "bigapplicator" && b.tag == "operator" && ppa == "func" && ppb == "inp") {
    // beta reduction
    let idx = undefined;
    // body
    for (let i = 0; i < b.names.length; i++) {
      if (b.names[i] == a.name) {
        idx = i;
        // link matching body
        relink(a, "inp", true, b, i, true);
      } else {
        // erase unused bodies
        let e = multiplexer("erase", NaN);
        relink(e, "val", false, b, i, true);
      }
    }
    // binding
    assert(idx >= 0);
    assert(b.arities[idx] == a.length);
    let ruleidx = 0, j = 0;
    for (let i = b.names.length; i < b.length; i++ , j++) {
      if (j >= b.arities[ruleidx]) {
        j = 0; ruleidx++;
      }
      if (ruleidx == idx) {
        // link match binders
        relink(b, i, true, a, j, true);
      } else {
        // erase unused vars
        let e = multiplexer("erase", NaN); e.reverse = true;
        relink(b, i, true, e, "val", false);
      }
    }
    return;
  }
  // commute
  stats.commute++;
  removeFromEnv(a);
  removeFromEnv(b);
  let pa = ports(a);
  pa.splice(pa.indexOf(ppa), 1);
  let pb = ports(b);
  pb.splice(pb.indexOf(ppb), 1);
  function makenodes(a, b, pb) {
    if (a.tag == "delimiter" && b.tag == "delimiter") {
      // remove original a delimiter
      relink(a, "inside", true, a, "outside", true, a.reverse);
      // keep b
      addEnv(b);
      return [b];
    }
    if (a.tag == "multiplexer" && b.tag == "delimiter") {
      a.ambientEnv = b.insideEnv;
      let e = b.insideEnv, o = a.labelEnv;
      if (!e.variant_envs[o.id]) {
        let arr = new Array(a.length);
        e.variant_envs[o.id] = arr;
        for (let p = 0; p < a.length; p++) {
          let en = make_real_env();
          en.is_variant_of = e;
          arr[p] = en;
          e.splitter = a;
        }
      }
    }
    let a_new = {};
    for (let p of pb) {
      let x = clone(a);
      if (b.tag == "multiplexer" && x.insideEnv)
        x.insideEnv = x.insideEnv.variant_envs[b.labelEnv.id][p];
      if (b.tag == "multiplexer" && x.labelEnv)
        x.labelEnv = x.labelEnv.variant_envs[b.labelEnv.n][p];
      addEnv(x);
      if (edge_dir(b, p) == edge_dir(b, ppb)) {
        // reverse link direction
        x.reverse = !x.reverse;
      }
      if (a.tag == "delimiter" && b.tag == "bigapplicator" && p != "inp" && x.full == "head") {
        x.full = "exit";
      }
      a_new[p] = x;
    }
    return a_new;
  }
  let a_new = makenodes(a, b, pb);
  let b_new = makenodes(b, a, pa);
  function makeOuterLinks(a, pa, b_new, ppb) {
    // link outside first to avoid overwriting inside
    for (let p of pa) {
      let x = b_new[p];
      if (a.tag == "multiplexer" && x.tag == "delimiter") {
        // copy off parent envs if a variant got reparented
        let d = x;
        for (let [v, v_full] of x.env.v_diff_stack) {
          let d_outer = delimiter(0); setEnv(d_outer, v); d_outer.full = v_full; d_outer.reverse = d.reverse;
          link(d_outer, "inside", d, "outside", !d.reverse);
          d = d_outer;
        }
        relink(a, p, true, d, "outside", false, edge_dir(a, p) == "out");
      } else {
        relink(a, p, true, x, ppb, false, edge_dir(a, p) == "out");
      }
    }
  }
  makeOuterLinks(a, pa, b_new, ppb);
  makeOuterLinks(b, pb, a_new, ppa);
  // link inside
  for (let pb1 of pb) {
    for (let pa1 of pa) {
      link(a_new[pb1], pa1, b_new[pa1], pb1, edge_dir(a_new[pb1], pa1) != "out");
    }
  }
}

let Director = (e, name, index) => { return { tag: "director", env: e, name, index }; };
let Level = (e, index, directors) => { return { tag: "level", env: e, real_env: e, index, directors: directors ? directors : [], hidden_levels: [] }; };
let Stack = () => {
  return {
    tag: "stack",
    levels: [],
    scopes: 0,
    returns: 0,
    lambdas: 0,
    saved: new Map(),
    edge_ids: []
  };
};

function validLevel(l) {
  assert(l.tag == "level");
  assert(l.index >= 0);
  assert(l.directors.length <= 1);
  for (let d of l.directors) {
    assert(d.tag == "director");
  }
}

function validStack(stack) {
  assert(stack.tag == "stack");
  assert(stack.scopes >= 0);
  //  assert(stack.returns >= 0);
  assert(stack.lambdas >= 0);
  assert(stack.scopes == stack.levels.length);
  for (let l of stack.levels) {
    validLevel(l);
  }
  let s = showStack(stack);
  return s;
}
function showLevel(l) {
  let s = "";
  let b = l;
  if (b.directors.length) s += "(";
  s += b.index;
  for (let d of b.directors) {
    s += "LRABCD"[d.index];
  }
  if (b.directors.length) s += ")";
  return s;
}
function showStack(stack) {
  let s = "";
  s += stack.scopes;
  for (let l of stack.levels) {
    s += showLevel(l);
  }
  return s;
}
function levelExtension(a, b) {
  if (a.index !== b.index) return false;
  if (a.directors.length == b.directors.length) {
    for (let k = 0; k < a.directors.length; k++) {
      if (a.directors[k].index !== b.directors[k].index) return false;
    }
    return true;
  }
  return false;
}
function isStackExtension(sub, sup) {
  // vague imitation of "transparency" from the paper
  // just compare for equality
  if (sub.scopes !== sup.scopes) return false;
  if (sub.levels.length !== sup.levels.length) return false;
  for (let l = 0; l < sub.levels.length; l++) {
    if (!levelExtension(sub.levels[l], sup.levels[l])) {
      return false;
    }
  }
  return true;
}

function readback(source, dir, stack_orig) {
  return {};
  validStack(stack_orig);
  let stack = clone(stack_orig); stack.saved = new Map(stack.saved);
  stack.edge_ids = stack.edge_ids.concat([source[dir].id]);
  let [term, from_dir] = follow(source, dir);
  switch (term.tag) {
    case "bigapplicator":
      if (from_dir == "inp") {
        let t = { tag: "bigapp", func: readback(term, "func", stack), terms: [] };
        for (let x = 0; x < term.length; x++) {
          t.terms.push(readback(term, x, stack));
        }
        return t;
      }
      assert(false);
    case "operator": {
      if (from_dir == "inp") {
        stack.lambdas++;
        let t = { tag: "operator", cases: [] };
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
        let b = stack.returns;
        return {
          tag: "symbol", idx: b, rule: term.names[j], rule_idx: i
        };
      }
      assert(false);
    }
    case "delimiter": {
      let e = term.env;
      assert(term.level == 0);
      if (from_dir == "outside") {
        if (!e.stack) e.stack = stack_orig;
        isStackExtension(e.stack, stack);

        if (stack.scopes > 0) {
          assert(e.parents.has(stack.levels[0].env));
        } else {
          assert(e.parents.has(all_envs[0]));
        }
        stack.scopes++;
        stack.levels = [Level(e, stack.scopes)].concat(stack.levels);
        if (term.full == "full") {
          assert(stack.saved.has(e));
          stack.levels[0].directors = [stack.saved.get(e)];
          stack.saved.delete(e);
        } else {
          stack.saved.delete(e);
        }
        return readback(term, "inside", stack);
      }
      if (from_dir == "inside") {
        let l = stack.levels[0];
        assert(l.env == e);
        assert(l.directors.length == (term.full == "full" ? 1 : 0));
        if (l.directors.length >= 1) {
          assert(!stack.saved.has(e));
          stack.saved.set(e, l.directors[0]);
        }
        stack.scopes--;
        stack.levels = stack.levels.slice(1);
        if (!e.stack) e.stack = stack_orig;
        isStackExtension(e.stack, stack);

        // silently pop off hidden levels

        return readback(term, "outside", stack);
      }
      assert(false);
    }
    case "multiplexer": {
      let i = term.level, e = term.originalEnv;
      let levels = Array.from(stack.levels); stack.levels = levels;
      if (from_dir == "val") {
        // σ[(jLδ)l]_i -> σ[jδl]_i
        let d;
        if (levels[i].directors.length > 0) {
          assert(levels[i].directors.length == 1);
          let l = clone(levels[i]);
          d = l.directors[0];
          l.directors = l.directors.slice(1);
          levels[i] = l;
        } else assert(false);
        assert(d.name == term.name);
        assert(d.env == term.originalEnv);
        let ie = term.env;
        // refine each env to its variant
        let newsublevels = [];
        for (let qq = 0; qq < i; qq++) {
          let l = clone(levels[qq]);
          assert(ie == l.env);
          l.env = l.env.variant_envs[e.n][d.index];
          newsublevels.push(l);
          let len = l.hidden_levels[e.n] ? l.hidden_levels[e.n].length : 0;
          assert(len == l.env.v_diff_stack.length);
          for (let hq = 0; hq < len; hq++) {
            let hl = l.hidden_levels[e.n][hq];
            assert(hl.env == l.env.v_diff_stack[hq][0]);
            assert(hl.directors.length == (l.env.v_diff_stack[hq][1] == "full" ? 1 : 0));
            newsublevels.push(hl);
            stack.scopes++;
          }
          delete l.hidden_levels[e.n];
          ie = ie.parent;
        }
        levels.splice(0, i, ...newsublevels);
        return readback(term, d.index, stack);
      } else {
        // σ[jδl]_i -> σ[(jLδ)l]_i
        let ie = term.env;
        // pack variant stack to match original
        for (let qq = 0; qq < i; qq++) {
          let l = clone(levels[qq]); levels[qq] = l;
          let ll = l.env.is_variant_of;
          assert(l.env == ll.variant_envs[e.n][from_dir]);
          assert(ie == ll);
          let vd = l.env.v_diff_stack;
          ie = ie.parent;
          for (let hq = 0; hq < vd.length; hq++) {
            let hl = levels[qq + hq + 1];
            assert(hl.env == vd[hq][0]);
            assert(hl.directors.length == (vd[hq][1] == "full" ? 1 : 0));
            stack.scopes--;
          }
          l.env = ll;
          l.hidden_levels[e.n] = levels.splice(qq + 1, vd.length);
        }
        let l = clone(levels[i]);
        assert(l.env == term.originalEnv);
        assert(l.directors.length == 0);
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