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

constant = (name, inp) => { return { tag: "constant", name, inp, length: 0 } };
cAse = (names, inp, out) => { return { tag: "case", names, inp, out, length: 0 } };

operator = (names, inp) => {
  return { tag: "operator", names, inp, length: 0 };
};

let edge_id = 0;
edge = (a, a_dir, b, b_dir) => { return { tag: "edge", a, a_dir, b, b_dir, active: false, id: edge_id++ } };

function ports(node) {
  let r;
  switch (node.tag) {
    case "initiator": return ["out"];
    case "applicator": return ["inp", "func", "arg"];
    case "abstractor": return ["inp", "body", "bind"];
    case "delimiter": return ["outside", "inside"];
    case "constant": r = ["inp"]; break;
    case "multiplexer": r = ["val"]; break;
    case "case": r = ["inp", "out"]; break;
    case "operator": r = ["inp"]; break;
  }
  for (let i = 0; i < node.length; i++)
    r.push(i);
  return r;
}

let in_dirs = new Set(["inp", "bind", "inside"]);
let out_dirs = new Set(["func", "arg", "body", "out", "outside", "val"]);
function check_edge(dir, is_in) {
  if (is_in) {
    if (0 <= dir && dir <= 20) return true;
    assert(in_dirs.has(dir));
  } else {
    assert(out_dirs.has(dir));
  }
}

function primaryPort(node) {
  switch (node.tag) {
    case "initiator": return undefined;
    case "applicator": return "func";
    case "case": return "out";
    default:
      return ports(node)[0];
  }
}
let activePairs = [];
function link(a, a_dir, b, b_dir) {
  if (b[b_dir])
    b[b_dir].active = false;
  if (a[a_dir])
    a[a_dir].active = false;
  let e = edge(a, a_dir, b, b_dir);
  b[b_dir] = a[a_dir] = e;
  if (primaryPort(a) == a_dir && primaryPort(b) == b_dir) {
    e.active = true;
    activePairs.push(e);
  }
  check_edge(a_dir, a.reverse);
  check_edge(b_dir, !b.reverse);
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
function compile(term) {
  let env = new Map();
  let stack = [];
  function resolve(term, parent, parent_dir) {
    switch (term.tag) {
      case "app":
        let a = applicator();
        link(parent, parent_dir, a, "inp");
        resolve(term.func, a, "func");
        resolve(term.arg, a, "arg");
        return a;
      case "abs":
        let l = abstractor(term.name);
        link(parent, parent_dir, l, "inp");

        let d2 = delimiter(0);
        link(d2, "outside", l, "bind");
        let v = multiplexer(term.name, 0);
        link(v, "val", d2, "inside");

        let d1 = delimiter(0); d1.reverse = true;
        link(l, "body", d1, "outside");

        if (env.has(v.name)) {
          env.get(v.name).push(v);
        } else {
          env.set(v.name, [v]);
        }
        stack.push(v);

        resolve(term.body, d1, "inside");

        stack.pop();
        env.get(v.name).pop();
        return l;
      case "symbol":
        if (env.has(term.name)) {
          let x = env.get(term.name);
          if (x.length) {
            let v = x[x.length - 1];
            for (let i = stack.length - 1; i >= 0; i--) {
              if (v == stack[i])
                break;
              let d = delimiter(0);
              link(parent, parent_dir, d, "inside");
              parent = d;
              parent_dir = "outside";
            }
            let l = v.length++;
            link(parent, parent_dir, v, l);
            return v;
          }
        }
        let c = constant(term.name);
        link(parent, parent_dir, c, "inp");
        return c;
      case "let": {
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
      }
      case "case":
        let cs = cAse([]);
        link(parent, parent_dir, cs, "inp");
        for (let [pat, rhs] of term.cases) {
          cs.names.push(pat.constr);
          let e = rhs;
          for (let v of pat.vars) {
            e = { tag: "abs", name: v, body: e };
          }
          let l = cs.length++;
          resolve(e, cs, l);
        }
        resolve(term.exp, cs, "out") // compile the scrutinee
        return cs;
    }
  }
  let i = initiator();
  resolve(term, i, "out");
  return i;
}


Block = (index, directors) => { return { index, directors: directors ? directors : [] }; };
Level = (block, rest) => { return { block, rest: rest ? rest : [] }; };
Stack = (bodyindex, levels) => { return { bodyindex, levels }; };

function validStack(stack) {
  assert(stack.bodyindex >= 0);
  for (let l of stack.levels) {
    assert(l.block);
    assert(l.rest);
    for (let b of [l.block].concat(l.rest)) {
      assert(b.index >= 0);
      for (let d of b.directors) {
        assert(d || d == 0);
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
        s += "LRABCD"[d];
      }
      if (b.directors.length) s += ")";
    }
    if (l.rest.length > 0) s += "]";
  }
  return s;
}

function readback(source, dir, stack) {
  let [term, from_dir] = follow(source, dir);
  validStack(stack);
  switch (term.tag) {
    case "applicator":
      if (from_dir == "inp") {
        return { tag: "app", func: readback(term, "func", stack), arg: readback(term, "arg", stack) };
      }
      assert(false);
    case "abstractor": {
      if (from_dir == "inp") {
        let i = stack.bodyindex;
        let newstack = Stack(i + 1, [Level(Block(i + 1))].concat(stack.levels));
        return { tag: "abs", body: readback(term, "body", newstack) };
      }
      if (from_dir == "bind") {
        return { tag: "symbol", name: stack.bodyindex - stack.levels[0].block.index };
      }
      assert(false);
    }
    case "delimiter": {
      if (term.level == 0 && from_dir == "outside")
        return readback(term, "inside", Stack(stack.bodyindex, [Level(Block(0))].concat(stack.levels)));
      if (term.level == 0 && from_dir == "inside")
        return readback(term, "outside", Stack(stack.bodyindex, stack.levels.slice(1)));
      let i = term.level - 1;
      // outside [ inside
      let levels = Array.from(stack.levels);
      if (from_dir == "outside") {
        // σ[bκl]_i -> σ[bl,κ]_i
        // split level into the stack
        let l = levels[i];
        let k = Level(l.rest[0]), lnew = Level(l.block, l.rest.slice(1));
        levels.splice(i, 1, lnew, k);
        return readback(term, "inside", Stack(stack.bodyindex, levels));
      } else if (from_dir == "inside") {
        // σ[bl,κ]_i -> σ[bκl]_i
        // AFAICT this means grouping two levels in the stack together
        let l1 = levels[i], l2 = levels[i + 1];
        let l_rest = [l2.block].concat(l2.rest, l1.rest);
        levels.splice(i, 2, Level(l1.block, l_rest));
        return readback(term, "outside", Stack(stack.bodyindex, levels));
      }
      assert(false);
    }
    case "multiplexer":
      let i = term.level;
      let levels = Array.from(stack.levels);
      if (from_dir == "val") {
        // σ[(jLδ)l]_i -> σ[jδl]_i
        let l = levels[i];
        let b = l.block;
        levels[i] = Level(Block(b.index, b.directors.slice(1)), l.rest);
        return readback(term, b.directors[0], Stack(stack.bodyindex, levels));
      } else {
        // σ[jδl]_i -> σ[(jLδ)l]_i
        let l = levels[i];
        let b = l.block;
        levels[i] = Level(Block(b.index, [from_dir].concat(b.directors)), l.rest);
        return readback(term, "val", Stack(stack.bodyindex, levels));
      }
    case "constant":
      return { tag: "symbol", name: term.name };
  }
  assert(false);
}

function getdot(root) {
  // traverse
  let visitedEdges = new Set();
  let visitedNodes = new Set();
  var stack = [root];
  for (let e of activePairs) {
    stack.unshift(e.a);
    stack.unshift(e.b);
    visitedEdges.add(e);
  }
  while (stack.length > 0) {
    let node = stack.pop();
    if (!node)
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
        } else {
          term = edge.a;
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
      switch (c.tag) {
        case "initiator": return `<span id=initiator>I</span>`;
        case "applicator": return "@";
        case "abstractor": return `λ<sub>${c.name}</sub>`;
        case "constant": return c.name;
        case "delimiter": return `${c.reverse ? "[" : "]"}${level}`;
        case "multiplexer": return `<span class=${c.reverse ? "demux" : "mux"}>${c.name}</span>${level}`;
        case "case": return "case";
        case "operator": return c.name;
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
    let is_active = edge.active ?
      "stroke-width: 3; "
      + (activePairs[activePairs.length - 1] == edge ? "stroke-dasharray: 2; " : "") : "";
    output += `"${a}" -> "${b}" [label="${edge.id}",`
      //      + (a_dir == "bind" ? `weight="0",` : "")
      + `style="${is_active}stroke: ${arrows[a_dir]}; fill: none",`
      + `arrowheadStyle="fill: ${arrows[b_dir]}"];\n`;
  }
  output += `}\n`;
  return output;
}
function eqNode(n1, n2) {
  if (n1.tag !== n2.tag) return false;
  if (n1.tag === "delimiter" && n1.level === n2.level) return true;

  if (n1.tag === "multiplexer" && n1.length === 0) assert(n1.level === 0);
  if (n1.tag === "multiplexer" && n1.level === n2.level) {
    assert(n1.length == n2.length);
    return true;
  }
  return false;
}
stats = { annihilate: 0, commute: 0 };
function updateLevel(a, b) {
  if (!a.hasOwnProperty("level")) return;
  if (b.tag == "abstractor") {
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
function rewriteStep() {
  let pair = activePairs.pop();
  assert(pair.active); while (!pair.active) pair = activePairs.pop();
  pair.active = false;
  let a = pair.a, b = pair.b;
  if (eqNode(a, b)) {
    // annihilate
    stats.annihilate++;
    let ppa = primaryPort(a);
    for (let p of ports(a)) {
      if (p == ppa) continue;
      let [ao, ado] = follow(a, p);
      let [bo, bdo] = follow(b, p);
      link(ao, ado, bo, bdo);
    }
    return;
  }
  if (a.tag == "applicator" && b.tag == "abstractor") {
    // beta reduction
    let [parent, pdir] = follow(a, "inp");
    let [body, bdir] = follow(b, "body");
    link(parent, pdir, body, bdir);

    let [a1, a1o] = follow(a, "arg");
    let [b1, b1o] = follow(b, "bind");
    link(b1, b1o, a1, a1o);
    return;
  }

  if (b.tag == "multiplexer" && b.length == 1) {
    // prune identity multiplexer
    let [bv, bvd] = follow(b, 0);
    link(a, pair.a_dir, bv, bvd);
    return;
  }
  if (a.tag == "multiplexer" && a.length == 1) {
    // prune identity multiplexer
    let [av, avd] = follow(a, 0);
    link(av, avd, b, pair.b_dir);
    return;
  }


  // commute
  stats.commute++;
  updateLevel(a, b); updateLevel(b, a);
  let pa = ports(a), ppa = primaryPort(a);
  pa.splice(pa.indexOf(ppa), 1);
  let pb = ports(b), ppb = primaryPort(b);
  pb.splice(pb.indexOf(ppb), 1);
  let a_new = {}, b_new = {};
  for (let p of pb) {
    a_new[p] = Object.assign({}, a);
    let [bo, bdo] = follow(b, p);
    if (b.tag == "abstractor" && p == "bind") {
      // reverse link direction
      a_new[p].reverse = !a_new[p].reverse;
      link(bo, bdo, a_new[p], ppa);
    } else {
      link(a_new[p], ppa, bo, bdo);
    }
  }
  for (let p of pa) {
    b_new[p] = Object.assign({}, b);
    let [ao, ado] = follow(a, p);
    if (a.tag == "applicator" && p == "arg") {
      b_new[p].reverse = !b_new[p].reverse;
      link(b_new[p], ppb, ao, ado);
    } else {
      link(ao, ado, b_new[p], ppb);
    }
  }
  for (let pb1 of pb) {
    for (let pa1 of pa) {
      if (b.tag == "abstractor" && pb1 == "bind" || a.tag == "applicator" && pa1 == "arg") {
        // reverse link direction
        link(a_new[pb1], pa1, b_new[pa1], pb1);
      } else {
        link(b_new[pa1], pb1, a_new[pb1], pa1);
      }
    }
  }
}