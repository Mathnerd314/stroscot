let multiplexer_parent_symbol = "⌧";
let explicit_lambda_delimiters = false;
let pop_scopes = true;

const assert = function (condition, message) {
  if (!condition) {
    debugger;
    console.log('Assert failed: ' + (message || ''));
  }
};

function clone(o) {
  let on = Object.assign({}, o);
  if(on.tag && (on.env || on.envs)) {
    setEnv(on, on.env || on.envs[0]);
  }
  return on;
}

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
function link(a, a_dir, b, b_dir, reverse) {
  if (reverse)
    return link(b, b_dir, a, a_dir);

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

let n_envs = 0, all_envs = [];
make_env = (parent) => {
  let e = {
    n: n_envs++,
    bindings: new Map(),
    parent,
    multiplexers: new Set(),
    delimiters: new Set(),
    binder: undefined
  };
  all_envs.push(e);
  return e;
};
function bind(env, v) {
  let name = v.name;
  assert(!env.bindings.has(name));
  env.bindings.set(name, v);
  setEnv(v, env);
}
function setEnv(t, env) {
  if(t.tag == "multiplexer") {
    env.multiplexers.add(t);
  } else if(t.tag == "delimiter") {
    env.delimiters.add(t);
  } else if(t.tag == "operator" || t.tag == "case") {
    assert(!explicit_lambda_delimiters);
    env.binder = t;
    env.delimiters.add(t);
    t.envs.push(env);
    return;
  } else if(t.tag == "level" || t.tag == "block") {
    return;
  } else {
    assert(false);
    return;
  }
  t.env = env;
}
function removeFromEnv(t, env) {
  if(t.tag == "multiplexer") {
    assert(env.multiplexers.has(t));
    env.multiplexers.delete(t);
  } else if(t.tag == "delimiter") {
    assert(env.delimiters.has(t));
    env.delimiters.delete(t);
  } else if(t.tag == "operator" || t.tag == "case") {
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
  resolve(undefined, term, i, "out");
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
        setEnv(o, new_env);
        o.names.push(constr);
        o.arities[o.length] = vars.length;
        for (let v_ of vars) {
          let v = multiplexer(v_, 0);
          muxes.push(v);
          bind(new_env, v);
        }
        if (explicit_lambda_delimiters) {
          let d1 = delimiter(0); d1.reverse = true; setEnv(d1, new_env);
          link(o, o.length++, d1, "outside");
          resolve(new_env, rhs, d1, "inside");
        } else
          resolve(new_env, rhs, o, o.length++);
      }
      for (let v of muxes) {
        if (explicit_lambda_delimiters) {
          let d2 = delimiter(0); setEnv(d2, v.env);
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
      cs.envs = [];
      link(parent, parent_dir, cs, "inp");
      res(term.exp, cs, "out") // compile the scrutinee
      let muxes = [];
      for (let { constr, vars, rhs } of term.cases) {
        cs.names.push(constr);
        cs.arities[cs.length] = vars.length;
        let new_env = make_env(env);
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
        assert(e.binder);
        let v = multiplexer(multiplexer_parent_symbol, 0); setEnv(v, e);
        link(parent, parent_dir, v, v.length++);
        let d = delimiter(0); setEnv(d, e);
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
Block = (e, index, directors) => { return { tag: "block", env: e, index, directors: directors ? directors : [] }; };
Level = (e, block, rest) => { return { tag: "level", env: e, block, rest: rest ? rest : [] }; };
Stack = () => { return { tag: "stack", bodyindex: 0, levels: [], scopes: 0, returns: 0, edge_ids: [] }; };

function validLevel(l) {
  assert(l.tag == "level");
  let b = l.block;
  assert(b.tag == "block");
  assert(b.index >= 0);
  for (let d of b.directors) {
    assert(d.tag == "director");
  }
  for (let lr of l.rest) {
    validLevel(lr);
  }
}

function validStack(stack) {
  assert(stack.tag == "stack");
  assert(stack.bodyindex >= 0);
  for (let l of stack.levels) {
    validLevel(l);
  }
  let s = showStack(stack);
  return s;
}
function showLevel(l) {
  let s = "";
  if (l.rest.length > 0) s += "[";
  let b = l.block;
  if (b.directors.length) s += "(";
  s += b.index;
  for (let d of b.directors) {
    s += "LRABCD"[d.index];
  }
  if (b.directors.length) s += ")";
  for (let lr of l.rest) {
    s += showLevel(lr);
  }
  if (l.rest.length > 0) s += "]";
  return s;
}
function showStack(stack) {
  let s = "";
  s += stack.bodyindex;
  for (let l of stack.levels) {
    s += showLevel(l);
  }
  return s;
}
function blockExtension(a, b, isFirst) {
  if (a.index !== b.index) return false;
  if (a.directors.length == b.directors.length) {
    for (let k = 0; k < a.directors.length; k++) {
      if (a.directors[k].index !== b.directors[k].index) return false;
    }
    return true;
  }
  if (!isFirst) return false;
  if (explicit_lambda_delimiters) {
    assert(a.directors.length == 1);
    assert(b.directors.length == 0);
    return true;
  } else {
    if (a.directors.length !== b.directors.length - 1) return false;
    for (let k = 0; k < a.directors.length; k++) {
      if (a.directors[k].index !== b.directors[k + 1].index) return false;
    }
    return true;
  }
  assert(false);
}
function levelExtension(a, b, isFirst) {
  if (!blockExtension(a.block, b.block, isFirst)) return false;
  if (a.rest.length !== b.rest.length) return false;
  for (let m = 0; m < a.rest.length; m++) {
    if (!levelExtension(a.rest[m], b.rest[m])) return false;
  }
  return true;
}
function isStackExtension(sub, sup) {
  // vague imitation of transparency from the paper
  // their explanation of substitution is longer than the explanation of the definition...

  if (sub.bodyindex !== sup.bodyindex && explicit_lambda_delimiters) return false;
  if (sub.scopes !== sup.scopes && explicit_lambda_delimiters) return false;
  if (sub.levels.length !== sup.levels.length) return false;
  for (let l = 0; l < sub.levels.length; l++) {
    if (!levelExtension(sub.levels[l], sup.levels[l], l === 0)) {
      return false;
    }
  }
  return true;
}

function readback(source, dir, stack) {
  stack = clone(stack);
  stack.edge_ids = stack.edge_ids.concat([source[dir].id]);
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
          let newstack = clone(stack);
          newstack.bodyindex++;
          if (!explicit_lambda_delimiters) {
            let e = es[x];
            newstack.levels = [Level(e, Block(e, newstack.bodyindex))].concat(stack.levels);
          }
          if (!term[x].bodyedgestack) term[x].bodyedgestack = [];
          term[x].bodyedgestack.push(newstack);
          t.cases.push([term.names[x], term.arities[x], readback(term, x, newstack)]);
          term[x].bodyedgestack.pop();
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
        let bodyedgestack = undefined;
        for (let st of term[j].bodyedgestack) {
          // if (st.bodyindex != bindindex) continue;
          if (!isStackExtension(st, stack)) continue;
          bodyedgestack = st;
          break;
        }
        if (!bodyedgestack) {
          assert(false);
          bodyedgestack = term[j].bodyedgestack[0];
          isStackExtension(bodyedgestack, stack);
        }
        let b;
        if (explicit_lambda_delimiters) {
          assert(bodyedgestack); // doesn't check returns
          assert(stack.returns >= 0);
          b = stack.returns - bodyedgestack.returns;
        } else {
          let e = es[j];
          let bindindex = stack.levels[0].block.index;
          assert(bodyedgestack);
          assert(stack.levels[0].env == e);
          assert(stack.levels[0].block.env == e);
          assert(stack.levels[0].block.directors.length == 1); // since we don't elide unit multiplexers
          assert(stack.levels[0].block.directors[0].env == e);
          b = stack.bodyindex - bindindex;
        }
        return {
          tag: "symbol", idx: b, rule: term.names[j], rule_idx: i
        };
      }
      assert(false);
    }
    case "delimiter": {
      let e = term.env;
      let newstack = clone(stack);
      if (term.level == 0 && from_dir == "outside") {
        newstack.levels = [Level(e, Block(e, newstack.scopes))].concat(stack.levels);
        if (explicit_lambda_delimiters)
          newstack.scopes++;
        return readback(term, "inside", newstack);
      }
      if (term.level == 0 && from_dir == "inside") {
        let l = stack.levels[0];
        if (!pop_scopes) {
          assert(l.env == e);
          assert(l.block.env == e);
          assert(l.block.directors.length <= 1);
          assert(l.block.directors.length == 0 || l.block.directors[0].env == e);
        }
        if (explicit_lambda_delimiters) {
          assert(l.block.directors.length == 1);
          newstack.scopes--;
          assert(newstack.scopes == l.block.index);
        }
        newstack.levels = stack.levels.slice(1);
        return readback(term, "outside", newstack);
      }
      let i = term.level - 1;
      // outside [ inside
      let levels = Array.from(newstack.levels); newstack.levels = levels;
      if (from_dir == "outside") {
        // σ[bκl]_i -> σ[bl,κ]_i
        // split a level back into the stack
        let l = clone(levels[i]), k = l.rest[0];
        if (!pop_scopes) {
          assert(k.env == e);
          assert(k.block.env == e);
          assert(k.block.directors.length === 0 || k.block.directors[0].env == e);
        }
        l.rest = l.rest.slice(1);
        levels.splice(i, 1, l, k);
        return readback(term, "inside", newstack);
      } else if (from_dir == "inside") {
        // σ[bl,κ]_i -> σ[bκl]_i
        // level κ+1 gets stored for safekeeping in level i
        let l1 = clone(levels[i]), l2 = levels[i + 1];
        if (!pop_scopes) {
          assert(l2.env === e);
          assert(l2.block.env === e);
          assert(l2.block.directors.length === 0 || l2.block.directors[0].env === e);
        }
        l1.rest = [l2].concat(l1.rest);
        levels.splice(i, 2, l1);
        return readback(term, "outside", newstack);
      }
      assert(false);
    }
    case "multiplexer": {
      let i = term.level, e = term.env;
      let newstack = clone(stack);
      let levels = Array.from(newstack.levels); newstack.levels = levels;
      if (from_dir == "val") {
        // σ[(jLδ)l]_i -> σ[jδl]_i
        let l = clone(levels[i]);
        let b = clone(l.block);
        let d = b.directors[0];
        assert(d.name == term.name);
        assert(d.env == e);
        b.directors = b.directors.slice(1);
        l.block = b;
        levels[i] = l;
        if (term.name == multiplexer_parent_symbol && explicit_lambda_delimiters) {
          newstack.bodyindex++;
          newstack.returns--;
        }
        return readback(term, d.index, newstack);
      } else {
        // σ[jδl]_i -> σ[(jLδ)l]_i
        let l = clone(levels[i]);
        let b = clone(l.block);
        if (!pop_scopes) {
          assert(b.env == e);
          assert(l.env == e);
          assert(b.directors.length == 0);
        }
        b.directors = [Director(e, term.name, from_dir)].concat(b.directors);
        l.block = b;
        levels[i] = l;
        if (term.name == multiplexer_parent_symbol && explicit_lambda_delimiters) {
          newstack.bodyindex--;
          newstack.returns++;
        }
        return readback(term, "val", newstack);
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
        envs.add(e.n);
    if (node.env)
      envs.add(node.env.n);

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
  envs = Array.from(envs);
  envs.sort((a,b) => b-a);

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
        case "delimiter": return `${c.reverse ? "ᙁ" : "ᙀ"}${level}`;
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
    if (c.env) env_num = c.env.n + 1;
    if (c.envs) env_num = c.envs[0].n + 1;
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
      arrowheadStyle: "fill: ${arrows[b_dir]}"
    }, edge.id);
  }
  envs.forEach((e, idx) => {
    g.setNode(num_nodes, {
      shape: "rect",
      labelType: "html",
      label: "" + e,
      style: `fill: ${colormap[e + 1]}; stroke: gray;`
    });
    g.setEdge(num_nodes, idx == 0 ? 0 : num_nodes - 1, {});
    num_nodes++;
  });
  return g;
}
function eqNode(n1, n2) {
  if (n1.tag !== n2.tag) return false;
  if (n1.tag === "delimiter" && n1.level === n2.level) {
    if (n1.level == 0 && !pop_scopes) {
      return false;
    }
    return true;
  }

  if (n1.tag === "multiplexer" && n1.length === 0) assert(n1.level === 0 || isNaN(n1.level));
  if (n1.tag === "multiplexer" && n1.level === n2.level) {
    assert(n1.length == n2.length);
    assert(n1.name === n2.name);
    assert(n1.env.n === n2.env.n);
    return true;
  }
  return false;
}
stats = { annihilate: 0, commute: 0 };
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


  if (explicit_lambda_delimiters) {
    if (b.tag == "delimiter" && b_dir == "outside") {
      extrudeDelimiter(b, a, a_dir);
      return;
    } else if (a.tag == "delimiter" && a_dir == "outside") {
      extrudeDelimiter(a, b, b_dir);
      return;
    }
  }
  link(a, a_dir, b, b_dir);
}

function updateLevels(a, b) {
  let has_level = a => a.tag === "delimiter" || a.tag === "multiplexer";
  if (b.tag === "operator") {
    if (!explicit_lambda_delimiters) {
      if (has_level(a))
        a.level++;
    }
    return;
  }
  if (b.tag == "delimiter") {
    if (a.tag == "multiplexer") {
      if (a.level >= b.level)
        a.level++;
      return;
    }
    if (a.tag == "delimiter") {
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
    assert(!a.hasOwnProperty("level"));
    return;
  }
  if (a.tag == "delimiter" && b.tag == "multiplexer") {
    if (a.level <= b.level)
      b.level++;
    return;
  }
  assert(a.tag !== "operator" && a.tag !== "delimiter" &&
    b.tag !== "operator" && b.tag !== "delimiter");
  return;
}

function extrudeDelimiter(d, g, g_dir, moved) {
  assert(explicit_lambda_delimiters);
  let t = g.tag, is_opening = d.reverse, stable = false;
  if (t == "operator") {
    if (g_dir == "inp") {
      // continue extruding along body
      assert(!is_opening);
      for (let i = 0; i < g.length; i++) {
        let [f, f_dir] = follow(g, i);
        let dn = clone(d);
        if (i >= g.arities.length)
          dn.reverse = !dn.reverse;
        extrudeDelimiter(dn, f, f_dir, true);
      }
    } else if (g_dir < g.arities.length) {
      // opening delimiter stopped at lambda body edge
      assert(is_opening);
    } else {
      // closing delimiter stopped at lambda binding edge
      assert(!is_opening);
      // disappear
    }
  } else if (t == "delimiter") {
    if (g_dir == "outside") {
      // another scope, go inside and continue until we get an endpoint
      d.level++;
      let [f, f_dir] = follow(g, "inside");
      extrudeDelimiter(d, f, f_dir, true);
    } else if (g_dir == "inside") {
      if (d.level > 0) {
        // exit scope
        d.level--; assert(d.level >= 0);
        let [f, f_dir] = follow(g, "outside");
        extrudeDelimiter(d, f, f_dir, true);
      } else {
        stable = true;
      }
    } else {
      assert(false);
    }
  } else if (t == "bigapplicator") {
    if (g_dir == "inp") {
      // split scope down applicator
      assert(!is_opening);
      for (let i of ["func", ...Array(g.length).keys()]) {
        let [f, f_dir] = follow(g, i);
        let dn = clone(d);
        extrudeDelimiter(dn, f, f_dir, true);
      }
    } else if (g_dir == "func") {
      // continue up
      assert(is_opening);
      let [f, f_dir] = follow(g, "inp");
      extrudeDelimiter(d, f, f_dir, true);
    } else {
      // should be alright? IDK
      // assert(false);
    }
  } else if (t == "constant") {
    // TODO
    assert(false);
  } else if (t == "multiplexer") {
    if (g_dir == "val") {
      // split
      if (d.level <= g.level)
        g.level++;
      for (let i = 0; i < g.length; i++) {
        let [f, f_dir] = follow(g, i);
        let dn = clone(d);
        extrudeDelimiter(dn, f, f_dir, true);
      }
    } else {
      // // continue up
      // let [f, f_dir] = follow(g, "val");
      // extrudeDelimiter(d, f, f_dir, true);
      // blocked
      stable = true;
    }
  } else if (t == "initiator") {
    assert(is_opening);
    stable = true;
  } else {
    assert(false);
    stable = true;
  }

  if (stable) {
    // scope boundary, insert delimiter
    if (moved) {
      relink(d, "inside", false, g, g_dir, true, !is_opening);
    }
    link(g, g_dir, d, "outside", !is_opening);
    return;
  } else {
    // disappear original delimiter
    if (!moved) {
      relink(g, g_dir, false, d, "inside", true, !is_opening);
    }
    return;
  }
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
    removeFromEnv(a, a.env);
    removeFromEnv(b, b.env);
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
        if (explicit_lambda_delimiters) {
          relink(a, "inp", true, b, i, true);
        } else {
          let d1 = delimiter(0); d1.reverse = true; setEnv(d1, env);
          relink(a, "inp", true, d1, "outside", false);
          relink(d1, "inside", false, b, i, true);
        }
      } else {
        // erase unused bodies
        let e = multiplexer("erase", NaN);
        relink(e, "val", false, b, i, true);
      }
    }
    assert(env);
    removeFromEnv(b, b.envs[0]);
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
  updateLevels(a, b);
  let pa = ports(a), ppa = primaryPort(a);
  pa.splice(pa.indexOf(ppa), 1);
  let pb = ports(b), ppb = primaryPort(b);
  pb.splice(pb.indexOf(ppb), 1);
  let a_new = {}, b_new = {};
  for (let p of pb) {
    a_new[p] = clone(a);
    if (b.tag == "operator" && p >= b.arities.length) {
      // reverse link direction
      a_new[p].reverse = !a_new[p].reverse;
    }
  }
  for (let p of pa) {
    b_new[p] = clone(b);
    if (a.tag == "bigapplicator" && p !== "inp") {
      b_new[p].reverse = !b_new[p].reverse;
    }
  }
  if(a.env || a.envs)
    removeFromEnv(a,a.env || b.envs[0]);
  if(b.env || b.envs)
    removeFromEnv(b,b.env || b.envs[0]);

  // link outside first to avoid overwriting inside
  for (let p of pb) {
    relink(a_new[p], ppa, false, b, p, true, (b.tag == "operator" && p >= b.arities.length));
  }
  for (let p of pa) {
    relink(a, p, true, b_new[p], ppb, false, (a.tag == "bigapplicator" && p !== "inp"));
  }
  for (let pb1 of pb) {
    for (let pa1 of pa) {
      link(b_new[pa1], pb1, a_new[pb1], pa1,
        ((b.tag == "operator" && pb1 >= b.arities.length) || (a.tag == "bigapplicator" && pa1 !== "inp")));
    }
  }
}
