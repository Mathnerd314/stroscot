'use strict';
var stats = { reductions: 0, annihilate: 0, commute: 0, beta: 0 }; window.stats = stats;

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

let multiplexer = (name, level, val) => { return { tag: "multiplexer", name, level, val, length: 0 } };
let delimiter = (level, inside, outside) => { return { tag: "delimiter", level, inside, outside } };

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
    case "bigapplicator": r = ["func", "inp"]; break;
    case "constant": r = ["inp"]; break;
    case "multiplexer": r = ["val"]; if (node.jump) r.push("jump"); break;
    case "delimiter": return ["outside", "inside"];
    case "case": r = ["out", "inp"]; break;
    case "operator": r = ["inp"]; break;
    default: assert(false);
  }
  for (let i = 0; i < node.length; i++)
    r.push(i);
  return r;
}

function native_edge_dir(t, dir) {
  if (0 <= dir) {
    assert(dir < t.length);
    if (t.tag == "case" || t.tag == "operator") {
      return (dir >= t.arities.length) ? "in" : "out";
    }
    if (t.tag == "bigapplicator" || t.tag == "constant") {
      return "out";
    }
    if (t.tag == "multiplexer") {
      return "in";
    }
    if (t.tag == "jumpanchor") return "in";
    if (t.tag == "jumpsource") return "out";

    assert(false);
  }

  if ((t.tag == "jumpanchor" || t.tag == "jumpsource" || t.tag == "bigapplicator" || t.tag == "operator") && dir == "inp"
    || t.tag == "operator" && dir == "bind"
    || t.tag == "delimiter" && dir == "outside")
    return "in";
  if (t.tag == "operator" && dir == "body"
    || t.tag == "bigapplicator" && dir == "func"
    || (t.tag == "jumpanchor" || t.tag == "jumpsource" || t.tag == "initiator") && dir == "out"
    || t.tag == "delimiter" && dir == "inside"
    || t.tag == "multiplexer" && dir == "val"
    || t.tag == "multiplexer" && dir == "jump")
    return "out";

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
  if (primaryPort(a) == a_dir && primaryPort(b) == b_dir) {
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

let n_binding_envs = 0;
function make_env(parent) {
  let e = {
    n: n_binding_envs++,
    bindings: new Map(),
    parent,
  };
  return e;
}
function bind(env, v) {
  let name = v.name;
  assert(!env.bindings.has(name));
  env.bindings.set(name, v);
  addEnv(v);
}
function addEnv(t) {
  // clear edges
  for (let p of ports(t)) {
    t[p] = null;
  }
}
function removeFromEnv(t) {
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
      let binds = [];
      for (let { constr, vars, rhs } of term.cases) {
        let new_env = make_env(env);
        o.names.push(constr);
        o.arities[o.length] = vars.length;
        for (let v_ of vars) {
          let v = multiplexer(v_, 0);
          bind(new_env, v);
          binds.push(v);
        }
        resolve(new_env, rhs, o, o.length++);
      }
      for (let v of binds) {
        link(v, "val", o, o.length++);
      }
      return o;
    }
    case "symbol": {
      let e = env, name = term.name;
      while (e && !(e.bindings.has(name))) {
        e = e.parent;
        let d = delimiter(0); d.reverse = true;
        link(parent, parent_dir, d, "inside");
        parent = d;
        parent_dir = "outside";
      }
      assert(e);
      let v = e.bindings.get(name);
      link(parent, parent_dir, v, v.length++);
      return v;
    }
    case "let": {
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

  let g = new Graph({ multigraph: true });
  g.setGraph({});
  for (let c of visitedNodes) {
    let shape = (() => {
      switch (c.tag) {
        case "initiator": return "circle"; //"doublecircle";
        case "applicator": return "circle";
        case "abstractor": return "rect";
        //        case "constant": return "star";
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
        case "bigapplicator": return `@${name}`;
        case "constant": return c.name;
        case "multiplexer": return `<span class=${c.reverse ? "demux" : "mux"}>${c.name}</span>${level}`;
        case "delimiter": return `${c.reverse ? "]" : "["}${level}`;
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
    if (false) {
      attrs.labelStyle = `border: solid 2px ${colormap[c.labelEnv.id]}`;
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
  return g;
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

function getports(a, ppa) {
  let pa = ports(a);
  pa.splice(pa.indexOf(ppa), 1);
  return pa;
}



function updateLevels(a, b, ppa, ppb) {
  let has_level = a => a.tag === "delimiter" || a.tag === "multiplexer";
  if (b.tag === "operator") {
    if (has_level(a))
      a.level++;
    return;
  }
  if (b.tag == "delimiter") {
    if (a.tag == "multiplexer") {
      if (a.level >= b.level)
        a.level++;
      return;
    }
    if (a.tag == "delimiter") {
      if (ppa == "outside" && ppb == "outside") {
        let i = a.level, j = b.level;
        if (i < j) {
          b.level++;
          return;
        } else if (i > j) {
          a.level++;
          return;
        }
        assert(i === 0 && j === 0 && !pop_scopes);
        // extruding [a][b] -> [[ab]]
        // it's almost arbitrary which scope is the outer,
        // but preserving a results in stack underflow, so keep b's [
        a.level++;
        return;
      }
      assert(false);
    }
    assert(!a.hasOwnProperty("level"));
    return;
  }
  if (a.tag == "delimiter" && b.tag == "multiplexer") {
    if (a.level <= b.level)
      b.level++;
    return;
  }
  if (a.tag == "delimiter" && b.tag == "bigapplicator") {
    return;
  }
  assert(a.tag !== "operator" && a.tag !== "delimiter" &&
    b.tag !== "operator" && b.tag !== "delimiter");
  return;
}

function makenodes(a, pa, b, pb, ppb) {
  let a_new = {};
  for (let p of pb) {
    let x;
    {
      x = clone(a);
      addEnv(x);
      if (edge_dir(b, p) == edge_dir(b, ppb)) {
        // reverse link direction
        x.reverse = !x.reverse;
      }
    }
    a_new[p] = x;
  }
  return a_new;
}

function makeOuterLinks(a, pa, b_new, ppb) {
  // link outside first to avoid overwriting inside
  for (let p of pa) {
    let x = b_new[p];
    relink(a, p, true, x, ppb, false, edge_dir(a, p) == "out");
  }
}
function eqNode(a, b) {
  if (a.tag !== b.tag) return false;
  if (a.tag === "multiplexer") {
    return a.level === b.level && a.length === b.length;
  }
  if (a.tag === "delimiter") {
    return a.level === b.level;
  }
  return false;
}

function reducePair(pair) {
  assert(pair.active); pair.active = false;
  let a = pair.a, b = pair.b, ppa = pair.a_dir, ppb = pair.b_dir;
  removeFromEnv(a);
  removeFromEnv(b);
  if (eqNode(a, b) && ppa == ppb) {
    // annihilate
    stats.annihilate++;
    for (let p of getports(a, ppa)) {
      relink(a, p, true, b, p, true);
    }
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
        let d = delimiter(0);
        relink(a, "inp", true, d, "outside", false);
        relink(d, "inside", false, b, i, true);
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
        // link matching binder
        let d = delimiter(0); d.reverse = true;
        relink(d, "outside", false, a, j, true);
        relink(b, i, true, d, "inside", false);
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
  let pa = getports(a, ppa), pb = getports(b, ppb);
  updateLevels(a, b, ppa, ppb);
  let a_new = makenodes(a, pa, b, pb, ppb);
  let b_new = makenodes(b, pb, a, pa, ppa);
  makeOuterLinks(a, pa, b_new, ppb);
  makeOuterLinks(b, pb, a_new, ppa);
  // link inside
  for (let pb1 of pb) {
    for (let pa1 of pa) {
      let kpa1 = pa1, kpb1 = pb1;
      link(a_new[pb1], kpa1, b_new[pa1], kpb1, edge_dir(a_new[pb1], kpa1) != "out");
    }
  }
}

let Director = (e, name, index) => { return { tag: "director", name, index }; };
let Level = (e, directors) => { return { tag: "level", env: e, directors: directors ? directors : [] }; };
let Stack = () => {
  return {
    tag: "stack",
    levels: [],
    edge_ids: [],
    lambdas: []
  };
};

function validLevel(l) {
  if (!l) return;
  assert(l.tag == "level");
  assert(l.directors.length <= 1);
  for (let d of l.directors) {
    assert(d.tag == "director");
  }
}

function validStack(stack) {
  assert(stack.tag == "stack");
  for (let l of stack.levels) {
    validLevel(l);
  }
  let s = showStack(stack);
  return s;
}
function showLevel(l) {
  let s = "";
  if (l) {
    if (l.directors.length) s += "(";
    s += l.env.id;
    for (let d of l.directors) {
      s += "LRABCD"[d.index];
    }
    if (l.directors.length) s += ")";
  }
  return s;
}
function showStack(stack) {
  let s = "";
  for (let l of stack.levels) {
    s += showLevel(l);
  }
  return s;
}
function levelExtension(a, b) {
  if (!a) return true;
  if (a.env !== b.env) return false;
  if (a.directors.length <= b.directors.length) {
    for (let k = 0; k < a.directors.length; k++) {
      if (a.directors[k].index !== b.directors[k].index) return false;
      if (a.directors[k].name !== b.directors[k].name) return false;
    }
    return true;
  }
  return false;
}
function isStackExtension(sub, sup) {
  // kind of compare for equality
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
  let stack = clone(stack_orig); stack.edge_ids = stack.edge_ids.concat([source[dir].id]);
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