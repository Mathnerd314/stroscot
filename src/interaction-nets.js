let multiplexer_parent_symbol = "⌧";
let explicit_lambda_delimiters = true;
let pop_scopes = true;
stats = { annihilate: 0, commute: 0, beta: 0 };

const assert = function (condition, message) {
  if (!condition) {
    debugger;
    console.log('Assert failed: ' + (message || ''));
  }
};

function clone(o) {
  let on = Object.assign({}, o);
  if (on.tag && (on.env || on.envs)) {
    setEnv(on, on.env || on.envs[0]);
  }
  return on;
}

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
    && !(a.tag == "delimiter" && b.tag == "delimiter" && a.full && b.full && a.env != b.env)) {
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
make_env = (parent) => {
  let e = {
    n: n_envs++,
    bindings: new Map(),
    parent, children: new Set(),
    multiplexers: new Set(),
    delimiters: new Set(),
    free: new Set()
  };
  all_envs.push(e);
  if (parent) {
    parent.children.add(e);
  }
  return e;
};
function bind(env, v) {
  let name = v.name;
  assert(!env.bindings.has(name));
  env.bindings.set(name, v);
  setEnv(v, env);
}
function setEnv(t, env) {
  if (t.tag == "multiplexer") {
    env.multiplexers.add(t);
    if (t.currentEnv) {
      assert(!t.currentEnv.free.has(t));
      t.currentEnv.free.add(t);
    }
  } else if (t.tag == "delimiter") {
    env.delimiters.add(t);
  } else if (t.tag == "operator" || t.tag == "case") {
    assert(!explicit_lambda_delimiters);
    env.binder = t;
    env.delimiters.add(t);
    t.envs.push(env);
    return;
  } else if (t.tag == "level" || t.tag == "block" || t.tag == "director") {
    return;
  } else {
    assert(false);
    return;
  }
  t.env = env;
}
function removeFromEnv(t) {
  let env;
  if (t.env) env = t.env;
  else return;
  if (t.tag == "multiplexer") {
    assert(env.multiplexers.has(t));
    env.multiplexers.delete(t);
    if (t.currentEnv) {
      assert(t.currentEnv.free.has(t));
      t.currentEnv.free.delete(t);
    }
  } else if (t.tag == "delimiter") {
    assert(env.delimiters.has(t));
    env.delimiters.delete(t);
  } else if (t.tag == "operator" || t.tag == "case") {
    assert(!explicit_lambda_delimiters);
    env.binder = undefined;
    assert(env.delimiters.has(t));
    env.delimiters.delete(t);
  } else {
    assert(false);
  }
}

function compile(term) {
  let i = initiator();
  i.root_env = make_env();
  resolve(i.root_env, term, i, "out");
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
      if (!explicit_lambda_delimiters)
        o.envs = [];
      link(parent, parent_dir, o, "inp");
      let muxes = [];
      for (let { constr, vars, rhs } of term.cases) {
        let new_env = make_env(env);
        if (!explicit_lambda_delimiters)
          setEnv(o, new_env);
        o.names.push(constr);
        o.arities[o.length] = vars.length;
        for (let v_ of vars) {
          let v = multiplexer(v_, 0);
          muxes.push(v);
          bind(new_env, v);
        }
        if (explicit_lambda_delimiters) {
          let d1 = delimiter(0); d1.reverse = true; setEnv(d1, new_env); d1.full = false;
          link(o, o.length++, d1, "outside");
          resolve(new_env, rhs, d1, "inside");
        } else
          resolve(new_env, rhs, o, o.length++);
      }
      for (let v of muxes) {
        if (explicit_lambda_delimiters) {
          let d2 = delimiter(0); setEnv(d2, v.env); d2.full = true;
          link(d2, "outside", o, o.length++);
          link(v, "val", d2, "inside");
        } else
          link(v, "val", o, o.length++);
      }
      return o;
    }
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
        if (!explicit_lambda_delimiters)
          setEnv(cs, new_env);
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
        assert(explicit_lambda_delimiters || e.binder);
        let v = multiplexer(multiplexer_parent_symbol, 0); setEnv(v, e);
        link(parent, parent_dir, v, v.length++);
        let d = delimiter(0); setEnv(d, e); d.full = true;
        link(v, "val", d, "inside");
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

Director = (e, name, index) => { return { tag: "director", env: e, name, index }; };
Level = (e, index, directors) => { return { tag: "level", env: e, index, directors: directors ? directors : [] }; };
Stack = () => {
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
          assert(e.parent == stack.levels[0].env);
        } else {
          assert(e.parent.n == 0);
        }
        stack.scopes++;
        stack.levels = [Level(e, stack.scopes)].concat(stack.levels);
        if (term.full) {
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
        assert(l.directors.length == (term.full ? 1 : 0));
        if (l.directors.length >= 1) {
          assert(!stack.saved.has(e));
          stack.saved.set(e, l.directors[0]);
        }
        stack.scopes--;
        stack.levels = stack.levels.slice(1);
        isStackExtension(e.stack, stack);
        return readback(term, "outside", stack);
      }
      assert(false);
    }
    case "multiplexer": {
      let i = term.level, e = term.env;
      let levels = Array.from(stack.levels); stack.levels = levels;
      if (from_dir == "val") {
        // σ[(jLδ)l]_i -> σ[jδl]_i
        let d;
        if (levels[i].directors.length > 0) {
          let l = clone(levels[i]);
          d = l.directors[0];
          l.directors = l.directors.slice(1);
          levels[i] = l;
        } else assert(false);
        assert(d.name == term.name);
        assert(d.env == e);
        if (term.name == multiplexer_parent_symbol && explicit_lambda_delimiters) {
          stack.returns--;
        }
        return readback(term, d.index, stack);
      } else {
        // σ[jδl]_i -> σ[(jLδ)l]_i
        let l = clone(levels[i]);
        assert(l.env == e);
        assert(l.directors.length == 0);
        l.directors = [Director(e, term.name, from_dir)].concat(l.directors);
        levels[i] = l;
        if (term.name == multiplexer_parent_symbol && explicit_lambda_delimiters) {
          stack.returns++;
        }
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

function replaceEnv(source, dir, stack, env_map, outer_env) {
  let child = (t, d) => replaceEnv(t, d, stack, env_map, outer_env);
  let [term, from_dir] = follow(source, dir);
  if (stack.has(term))
    return;
  stack.add(term);
  switch (term.tag) {
    case "bigapplicator":
      if (from_dir == "inp") {
        child(term, "func");
        for (let x = 0; x < term.length; x++) {
          child(term, x);
        }
        return;
      }
      assert(false);
    case "operator": {
      if (from_dir == "inp") {
        for (let x = 0; x < term.arities.length; x++) {
          child(term, x);
        }
        return;
      }
      if (from_dir >= term.arities.length) {
        return;
      }
      assert(false);
    }
    case "delimiter": {
      let e = term.env;
      if (!env_map.has(e)) {
        assert(env_map.has(e.parent));
        let p = env_map.get(e.parent);
        let e_new = make_env(p);
        env_map.set(e, e_new);
      }
      removeFromEnv(term);
      setEnv(term, env_map.get(e));
      if (e === outer_env)
        return;
      if (from_dir == "outside") {
        child(term, "inside");
        return;
      }
      if (from_dir == "inside") {
        child(term, "outside");
        return;
      }
      assert(false);
    }
    case "multiplexer": {
      let bound = env_map.has(term.env);
      let in_scope = term.currentEnv && env_map.has(term.currentEnv);
      assert(bound || in_scope);
      removeFromEnv(term);
      if (in_scope)
        term.currentEnv = env_map.get(term.currentEnv);
      if (bound)
        term.env = env_map.get(term.env);
      setEnv(term, term.env);
      if (from_dir == "val") {
        for (let i = 0; i < term.length; i++)
          child(term, i);
        return;
      } else {
        child(term, "val");
        return;
      }
    }
    case "constant": {
      for (let i = 0; i < term.length; i++) {
        child(term, i);
      }
      return;
    }
    case "case": {
      if (from_dir == "inp") {
        child(term, "out");
        for (let i = 0; i < term.arities.length; i++) {
          child(term, i);
        }
        return;
      }
      if (from_dir >= term.arities.length) {
        return;
      }
    }
  }
  assert(false);
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

    if (node.envs)
      for (let e of node.envs)
        envs.add(e);
    if (node.env)
      envs.add(node.env);

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
        case "delimiter": return `${c.reverse ? (c.full ? "ᙉ" : "ᙁ") : (c.full ? "ᙈ" : "ᙀ")}${level}`;
        case "multiplexer": return `<span class=${c.reverse ? "demux" : "mux"}>${c.name}</span>${level}`;
        case "case": return "case";
        case "operator":
          if (c.names.length == 1 && c.names[0] == "sole") {
            return `λ`;
          } else return `λ{${c.names}}`;
        default: assert(false);
      }
    })();
    let i = m.get(c);
    let env_num = 0;
    // if (c.env) env_num = envs.indexOf(c.env.n) + 1;
    // if (c.envs) env_num = envs.indexOf(c.envs[0].n) + 1;
    if (c.env) env_num = c.env.n; // indexOf?
    if (c.envs) env_num = c.envs[0].n;
    g.setNode(i, { shape, labelType: "html", label: human, style: `fill: white; stroke: ${colormap[env_num]};` });
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
  all_envs.forEach((e, idx) => {
    if (e.n == 0) return;
    g.setNode("e" + e.n, {
      shape: "rect",
      labelType: "html",
      label: "" + e.n,
      style: envs.has(e) ? `fill: ${colormap[e.n]}; stroke: gray;` : `fill: white; stroke: ${colormap[e.n]};`
    });
    g.setEdge("e" + e.n, e.parent.n == 0 ? 0 : "e" + e.parent.n, {});
  });
  return g;
}
function eqNode(n1, n2) {
  if (n1.tag !== n2.tag) return false;
  if (n1.tag === "delimiter" && n1.env === n2.env && n1.full && n2.full) return true;
  if (n1.tag === "multiplexer" && n1.length === 0) assert(n1.level === 0 || isNaN(n1.level));
  if (n1.tag === "multiplexer" && n1.level === n2.level) {
    assert(n1.length == n2.length);
    assert(n1.name === n2.name);
    assert(n1.env === n2.env);
    return true;
  }
  return false;
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

function relink(a, a_dir, af, b, b_dir, bf, reverse) {
  if (af)
    [a, a_dir] = follow(a, a_dir);
  if (bf)
    [b, b_dir] = follow(b, b_dir);
  if (reverse)
    [a, a_dir, b, b_dir] = [b, b_dir, a, a_dir];

  link(a, a_dir, b, b_dir);
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
    let idx = undefined, env;
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
        if (explicit_lambda_delimiters) {
          relink(b, i, true, a, j, true);
        } else {
          let d2 = delimiter(0); setEnv(d2, env);
          relink(b, i, true, d2, "inside", false);
          relink(d2, "outside", false, a, j, true);
        }
      } else {
        // erase unused vars
        let e = multiplexer("erase", NaN); e.reverse = true;
        relink(b, i, true, e, "val", false);
      }
    }
    return;
  }
  if (a.tag == "delimiter" && ppa == "outside" && b.tag == "delimiter" && ppb == "outside") {
    if (a.env == b.env) {
      let new_env = make_env(b.parent);
      replaceEnv(b, "inside", new Set(), new Map([[b.env, new_env]]), b.env);
    }

    // reparent env
    let e = b.env;
    e.parent.children.delete(e);
    e.parent = a.env;

    // find all other delimiters in env
    let freeDelimiters = new Set();
    for (let d of e.delimiters) {
      if (b !== d)
        freeDelimiters.add(d);
    }

    // add delimiter to outside of free delimiters - skip if already there
    for (let f of freeDelimiters) {
      let d = clone(a); d.reverse = f.reverse;
      relink(d, "outside", false, f, "outside", true, f.reverse);
      link(f, "outside", d, "inside", f.reverse);
    }

    // remove original
    assert(!a.reverse);
    relink(a, "inside", true, a, "outside", true, a.reverse);
    removeFromEnv(a);

    // bump all free multiplexers in env
    let estack = [e], ef;
    while (ef = estack.pop()) {
      for (let m of ef.free) {
        m.level++;
      }
      estack.push(...ef.children);
    }
    return;
  }

  // commute
  stats.commute++;
  removeFromEnv(a);
  removeFromEnv(b);
  let has_level = a => a.tag === "delimiter" || a.tag === "multiplexer";
  if (b.tag == "delimiter") {
    if (a.tag == "multiplexer") {
      a.level++;
      a.currentEnv = b.env;
    } else assert(!has_level(a));
  } else if (a.tag == "delimiter") {
    if (b.tag == "multiplexer") {
      b.level++;
      b.currentEnv = a.env;
    } else assert(!has_level(b));
  } else assert(a.tag !== "delimiter" && b.tag !== "delimiter");
  let pa = ports(a);
  pa.splice(pa.indexOf(ppa), 1);
  let pb = ports(b);
  pb.splice(pb.indexOf(ppb), 1);
  let a_new = {}, b_new = {};
  for (let p of pb) {
    a_new[p] = clone(a);
    if (edge_dir(b, p) == "in") {
      // reverse link direction
      a_new[p].reverse = !a_new[p].reverse;
    }
  }
  for (let p of pa) {
    b_new[p] = clone(b);
    if (edge_dir(a, p) == "out") {
      b_new[p].reverse = !b_new[p].reverse;
    }
  }

  // link outside first to avoid overwriting inside
  for (let p of pa) {
    relink(a, p, true, b_new[p], ppb, false, edge_dir(a, p) == "out");
  }
  for (let p of pb) {
    relink(a_new[p], ppa, false, b, p, true, edge_dir(b, p) == "in");
  }
  // link inside
  for (let pb1 of pb) {
    for (let pa1 of pa) {
      link(a_new[pb1], pa1, b_new[pa1], pb1, edge_dir(a_new[pb1], pa1) != "out");
    }
  }
}
