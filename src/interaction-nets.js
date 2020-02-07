const assert = function (condition, message) {
  if (!condition) {
    debugger;
    console.log('Assert failed: ' + (message || ''));
  }
};

applicator = (inp, func, arg) => { return { tag: "applicator", inp, func, arg } };
abstractor = (name, inp, body, bind) => { return { tag: "abstractor", name, inp, body, bind } };


delimiter = (level, inside, outside) => { return { tag: "delimiter", level, inside, outside } };
// in the initial graph inside is the parent, outside is the child.
// it faces up. so the graph looks like inside-]-outside.
// later lambda disintegrates to outside-[-inside and they eventually annihilate (hopefully)

multiplexer = (name, level, val) => { return { tag: "multiplexer", name, level, val, length: 0 } };
// eraser = multiplexer of length 0, duplicator = multiplexer of length 2

initiator = (out) => { return { tag: "initiator", out: out } };

// jumbo tuples
// (i, a, b, c)
constant = (name, inp) => { return { tag: "constant", name, inp, length: 0 } };
// Paul Levy uses pm or "pattern match" as often there is only one pattern
// But Haskell uses case consistently. And I think it fits better since
// pattern-matching also implies patterns, whereas these are only tuples.
// case M of {i a b. N}
cAse = (names, arities, inp, out) => { return { tag: "case", names, arities, inp, out, length: 0 } };

// A jumbo lambda-calculus function
// \{x. a, y. b}
operator = (names, arities, inp) => {
  return { tag: "operator", names, arities, inp, length: 0 };
};
// Node for a @ <i, N>
bigapplicator = (name, inp, func) => { return { tag: "bigapplicator", name, inp, func, length: 0 } };

let edge_id = 0;
edge = (a, a_dir, b, b_dir) => { return { tag: "edge", a, a_dir, b, b_dir, active: false, id: edge_id++ } };

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
function check_edge(t, dir, is_in) {
  if (0 <= dir) {
    if (t.tag == "case" || t.tag == "operator") {
      assert(is_in == (dir >= t.arities.length));
      return;
    }
    if (t.tag == "bigapplicator" || t.tag == "constant") {
      assert(!is_in);
      return;
    }
    if (t.tag == "multiplexer") {
      assert(is_in);
      return;
    }
    assert(false);
  }

  if (is_in) {
    assert(in_dirs.has(dir));
  } else {
    assert(out_dirs.has(dir));
  }
}

function primaryPort(node) {
  switch (node.tag) {
    case "initiator": return undefined;
    default:
      return ports(node)[0];
  }
}
let activePairs = [];
function link(a, a_dir, b, b_dir) {
  if (b[b_dir])
    b[b_dir].active = undefined;
  if (a[a_dir])
    a[a_dir].active = undefined;
  let e = edge(a, a_dir, b, b_dir);
  b[b_dir] = a[a_dir] = e;
  if (primaryPort(a) == a_dir && primaryPort(b) == b_dir) {
    e.active = true;
    activePairs.push(e);
  }
  check_edge(a, a_dir, !!a.reverse);
  check_edge(b, b_dir, !b.reverse);
}
function follow(source, dir) {
  let edge = source[dir], term, from_dir;
  if (source == edge.a && dir == edge.a_dir) {
    term = edge.b;
    from_dir = edge.b_dir;
  } else {
    term = edge.a;
    from_dir = edge.a_dir;
  }
  return [term, from_dir];
}

let n_envs = 0;
make_env = (parent) => {
  return {
    n: n_envs++,
    bindings: new Map(),
    parent
  }
};
function bind(env, v) {
  let name = v.name;
  assert(!env.bindings.has(name));
  env.bindings.set(name, v);
  v.env = env;
}

function compile(term) {
  let env = make_env();
  let i = initiator();
  resolve(env, term, i, "out");
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
      o.envs = [];
      link(parent, parent_dir, o, "inp");
      let muxes = [];
      for (let { constr, vars, rhs } of term.cases) {
        let new_env = make_env(env);
        new_env.binder = o; o.envs.push(new_env);
        o.names.push(constr);
        o.arities[o.length] = vars.length;
        for (let v_ of vars) {
          let v = multiplexer(v_, 0);
          muxes.push(v);
          bind(new_env, v);
        }
        resolve(new_env, rhs, o, o.length++);
      }
      for (let v of muxes) {
        link(v, "val", o, o.length++);
      }
      return o;
    }
    case "case": {
      let cs = cAse([], []);
      cs.arities.length = term.cases.length;
      link(parent, parent_dir, cs, "inp");
      res(term.exp, cs, "out") // compile the scrutinee
      let muxes = [];
      for (let { constr, vars, rhs } of term.cases) {
        cs.names.push(constr);
        cs.arities[cs.length] = vars.length;
        let new_env = make_env(env);
        // no binder for new_env
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
    case "symbol": {
      let e = env, name = term.name;
      while (e && !(e.bindings.has(name))) {
        if (e.binder) {
          let d = delimiter(0);
          d.env = e;
          link(parent, parent_dir, d, "inside");
          parent = d;
          parent_dir = "outside";
        }
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

Director = (e, name, index) => { return { tag: "director", env: e, name, index }; };
Block = (e, index, directors) => { return { tag: "block", env: e, index, directors: directors ? directors : [] }; };
Level = (e, block, rest) => { return { tag: "level", env: e, block, rest: rest ? rest : [] }; };
Stack = (bodyindex, levels) => { return { tag: "stack", bodyindex, levels }; };

function validStack(stack) {
  assert(stack.tag == "stack");
  assert(stack.bodyindex >= 0);
  for (let l of stack.levels) {
    assert(l.tag == "level");
    for (let b of [l.block].concat(l.rest)) {
      assert(b.tag == "block");
      assert(b.index >= 0);
      for (let d of b.directors) {
        assert(d.tag == "director");
      }
    }
  }
  let s = showStack(stack);
  return s;
}
function showStack(stack) {
  let s = "";
  s += stack.bodyindex;
  for (let l of stack.levels) {
    if (l.rest.length > 0) s += "[";
    for (let b of [l.block].concat(l.rest)) {
      if (b.directors.length) s += "(";
      s += b.index;
      for (let d of b.directors) {
        s += "LRABCD"[d.index];
      }
      if (b.directors.length) s += ")";
    }
    if (l.rest.length > 0) s += "]";
  }
  return s;
}

function subsetStack(sub, sup) {
  // transparency from the paper
  // ???
  /*
  let i = sub.bodyindex;
  sup.bodyindex = 0;

  sub <= sup
  */
}

function readback(source, dir, stack) {
  let [term, from_dir] = follow(source, dir);
  validStack(stack);
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
      let es = term.envs;
      if (from_dir == "inp") {
        let i = stack.bodyindex;
        let t = { tag: "operator", cases: [] };
        for (let x = 0; x < term.arities.length; x++) {
          assert(term.names[x]);
          let e = es[x];
          let newstack = Stack(i + 1, [Level(e, Block(e, i + 1))].concat(stack.levels));
          t.cases.push([term.names[x], term.arities[x], readback(term, x, newstack)]);
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
        let e  = es[j];
        assert(stack.levels[0].env == e);
        assert(stack.levels[0].block.env == e);
        assert(stack.levels[0].block.directors.length > 0); // since we don't elide unit multiplexers
        assert(stack.levels[0].block.directors[0].env == e);
        let b = stack.bodyindex - stack.levels[0].block.index;
        return {
          tag: "symbol", idx: b,
          rule: term.names[j], rule_idx: i, name: stack.levels[0].block.directors[0].name
        };
      }
      assert(false);
    }
    case "delimiter": {
      let e = term.env;
      if (term.level == 0 && from_dir == "outside")
        return readback(term, "inside", Stack(stack.bodyindex, [Level(e, Block(e, 0))].concat(stack.levels)));
      if (term.level == 0 && from_dir == "inside") {
        assert(stack.levels[0].env == e);
        assert(stack.levels[0].block.env == e);
        return readback(term, "outside", Stack(stack.bodyindex, stack.levels.slice(1)));
      }
      let i = term.level - 1;
      // outside [ inside
      let levels = Array.from(stack.levels);
      if (from_dir == "outside") {
        // σ[bκl]_i -> σ[bl,κ]_i
        // split a level into the stack
        let l = levels[i];
        let k = Level(e, l.rest[0]), lnew = Object.assign({}, l);
        assert(l.rest[0].env == e);
        lnew.rest = l.rest.slice(1);
        levels.splice(i, 1, lnew, k);
        return readback(term, "inside", Stack(stack.bodyindex, levels));
      } else if (from_dir == "inside") {
        // σ[bl,κ]_i -> σ[bκl]_i
        // AFAICT this means grouping two levels in the stack together (destructing a level)
        let l1 = Object.assign({}, levels[i]), l2 = levels[i + 1];
        assert(l2.env == e);
        l1.rest = [l2.block].concat(l2.rest, l1.rest);
        levels.splice(i, 2, l1);
        return readback(term, "outside", Stack(stack.bodyindex, levels));
      }
      assert(false);
    }
    case "multiplexer":
      let i = term.level, e = term.env;
      let levels = Array.from(stack.levels);
      if (from_dir == "val") {
        // σ[(jLδ)l]_i -> σ[jδl]_i
        let l = Object.assign({}, levels[i]);
        let b = Object.assign({}, l.block);
        let d = b.directors[0];
        assert(d.name == term.name);
        assert(d.env == e);
        b.directors = b.directors.slice(1);
        l.block = b;
        levels[i] = l;
        return readback(term, d.index, Stack(stack.bodyindex, levels));
      } else {
        // σ[jδl]_i -> σ[(jLδ)l]_i
        let l = Object.assign({}, levels[i]);
        let b = Object.assign({}, l.block);
        assert(b.env == e);
        assert(l.env == e);
        b.directors = [Director(e, term.name, from_dir)].concat(b.directors);
        l.block = b;
        levels[i] = l;
        return readback(term, "val", Stack(stack.bodyindex, levels));
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
      if (n in node && node[n]) {
        let edge = node[n], term;
        if (node == edge.a && n == edge.a_dir) {
          term = edge.b;
          assert(edge.active === undefined || term[edge.b_dir] == edge);
        } else {
          term = edge.a;
          assert(edge.active === undefined || term[edge.a_dir] == edge);
        }
        stack.push(term);
        visitedEdges.add(edge);
      }
    }
  }

  // Map of the things
  var c = 0;
  let m = new Map();
  for (let i of visitedNodes) {
    m.set(i, c++);
  }

  var output = `digraph {\n  node [style="fill: white; stroke: black"]\n`;
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
      let name = c.name ? `<sub>${c.name}</sub>` : "";
      switch (c.tag) {
        case "initiator": return `<span id=initiator>I</span>`;
        case "applicator": return `@`;
        case "bigapplicator": return `@${name}`;
        case "abstractor": return `λ${name}`;
        case "constant": return c.name;
        case "delimiter": return `${c.reverse ? "[" : "]"}${level}`;
        case "multiplexer": return `<span class=${c.reverse ? "demux" : "mux"}>${c.name}</span>${level}`;
        case "case": return "case";
        case "operator": return `λ{${c.names}}`;
        default: assert(false);
      }
    })();
    let i = m.get(c);
    output += `"${i}" [shape=${shape},labelType="html",label="${human}"];\n`;
  }
  let arrows = ["black", "red", "green", "blue", "purple", "yellow", "orange"];
  for (let edge of visitedEdges) {
    let a = m.get(edge.a);
    let a_dir = ports(edge.a).indexOf(edge.a_dir);
    let b = m.get(edge.b);
    let b_dir = ports(edge.b).indexOf(edge.b_dir);
    if (a_dir > arrows.length) a_dir = 0;
    if (b_dir > arrows.length) b_dir = 0;
    let is_active = edge.active ? "stroke-width: 3; " : "";
    let is_spine = redexEdges.has(edge) || (activePairs[activePairs.length - 1] == edge) ? "stroke-dasharray: 2; " : "";
    output += `"${a}" -> "${b}" [label="${edge.id}",`
      //      + (a_dir == "bind" ? `weight="0",` : "")
      + `style="${is_active}${is_spine}stroke: ${arrows[a_dir]}; fill: none",`
      + `arrowheadStyle="fill: ${arrows[b_dir]}"];\n`;
  }
  output += `}\n`;
  return output;
}
function eqNode(n1, n2) {
  if (n1.tag !== n2.tag) return false;
  if (n1.tag === "delimiter" && n1.level === n2.level) return true;

  if (n1.tag === "multiplexer" && n1.length === 0) assert(n1.level === 0 || isNaN(n1.level));
  if (n1.tag === "multiplexer" && n1.level === n2.level) {
    assert(n1.length == n2.length);
    return true;
  }
  return false;
}
stats = { annihilate: 0, commute: 0 };
function updateLevel(a, b) {
  if (!a.hasOwnProperty("level")) return;
  if (b.tag == "abstractor" || b.tag == "operator") {
    a.level++;
    return;
  }
  if (b.tag == "delimiter") {
    if (a.level >= b.level) {
      a.level++;
      return;
    }
  }
}

// BOHM: f.nform[i].nform[f.nport[i]] == f
// mine: [a,ad] = follow(f, i); f == follow(a,ad)

loStack = [];
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

function relink(a, a_dir, af, b, b_dir, bf) {
  if (af)
    [a, a_dir] = follow(a, a_dir);
  if (bf)
    [b, b_dir] = follow(b, b_dir);

  link(a, a_dir, b, b_dir);
}

function reducePair(pair) {
  assert(pair.active); pair.active = false;
  let a = pair.a, b = pair.b;
  if (eqNode(a, b)) {
    // annihilate
    stats.annihilate++;
    let ppa = primaryPort(a);
    for (let p of ports(a)) {
      if (p == ppa) continue;
      relink(a, p, true, b, p, true);
    }
    return;
  }
  if (a.tag == "bigapplicator" && b.tag == "operator") {
    // beta reduction
    let idx = undefined, env;
    // body
    for (let i = 0; i < b.names.length; i++) {
      if (b.names[i] == a.name) {
        idx = i; env = b.envs[i];
        // link matching body
        let d1 = delimiter(0); d1.reverse = true; d1.env = env;
        relink(a, "inp", true, d1, "outside", false);
        relink(d1, "inside", false, b, i, true);
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
        let d2 = delimiter(0); d2.env = env;
        relink(b, i, true, d2, "inside", false);
        relink(d2, "outside", false, a, j, true);
      } else {
        // erase unused vars
        let e = multiplexer("erase", NaN); e.reverse = true;
        relink(b, i, true, e, "val", false);
      }
    }
    return;
  }
  /*
    if (b.tag == "multiplexer" && b.length == 1) {
      // prune identity multiplexer
      relink(a, pair.a_dir, false, b, 0, true);
      return;
    }
    if (a.tag == "multiplexer" && a.length == 1) {
      // prune identity multiplexer
      relink(a, 0, true, b, pair.b_dir, false);
      return;
    }
  */
  // commute
  stats.commute++;
  updateLevel(a, b); updateLevel(b, a);
  let pa = ports(a), ppa = primaryPort(a);
  pa.splice(pa.indexOf(ppa), 1);
  let pb = ports(b), ppb = primaryPort(b);
  pb.splice(pb.indexOf(ppb), 1);
  let a_new = {}, b_new = {};
  for (let p of pb) {
    a_new[p] = pb.length == 1 ? a : Object.assign({}, a); // reuse object if only 1 (for root)
    if (b.tag == "operator" && p >= b.arities.length) {
      // reverse link direction
      a_new[p].reverse = !a_new[p].reverse;
      relink(b, p, true, a_new[p], ppa, false);
    } else {
      relink(a_new[p], ppa, false, b, p, true);
    }
  }
  for (let p of pa) {
    b_new[p] = pa.length == 1 ? b : Object.assign({}, b);
    if (a.tag == "bigapplicator" && p >= 0) {
      b_new[p].reverse = !b_new[p].reverse;
      relink(b_new[p], ppb, false, a, p, true);
    } else {
      relink(a, p, true, b_new[p], ppb, false);
    }
  }
  for (let pb1 of pb) {
    for (let pa1 of pa) {
      if (b.tag == "operator" && pb1 >= b.arities.length
        || a.tag == "bigapplicator" && pa1 >= 0) {
        // reverse link direction
        link(a_new[pb1], pa1, b_new[pa1], pb1);
      } else {
        link(b_new[pa1], pb1, a_new[pb1], pa1);
      }
    }
  }
}