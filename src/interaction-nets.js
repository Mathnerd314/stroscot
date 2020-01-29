let types =
  ["wire", "amb", "eqn"
    , "print", "read", "atom"
    , "apply", "lambda", "scope", "fan", "erase"
    // applicator, abstractor, delimiter, duplicator, eraser
    , "initiator", "multiplexer", "case", "operator"]; // graph-rewriting package

initiator = (out) => { return { tag: "initiator", out: out } };
applicator = (inp, func, arg) => { return { tag: "applicator", inp, func, arg } };
abstractor = (name, inp, body, bind) => { return { tag: "abstractor", name, inp, body, bind } };
delimiter = (level, inp, out) => { return { tag: "delimiter", level, inp, out } };
// out is the parent, inp is the child. so the graph looks like inp-[-out.

eraser = (inp) => { return { tag: "eraser", inp } };
duplicator = (level, inp, outs) => { return { tag: "duplicator", level, inp, outs } };
multiplexer = (name, level, val) => { return { tag: "multiplexer", name, level, val, length: 0 } };

constant = (name, inp, args) => { return { tag: "constant", name, inp, args } };

cAse = (names, inp, out) => { return { tag: "case", names, inp, out, length: 0 } };
operator = (inp, arity, lmop, func, name) => {
  return { tag: "operator", inp, arity, lmop, func, name, length: 0 };
};

edge = (a, a_dir, b, b_dir) => { return { tag: "edge", a, a_dir, b, b_dir } };

function ports(node) {
  let r;
  switch (node.tag) {
    case "initiator": return ["out"];
    case "applicator": return ["inp", "func", "arg"];
    case "abstractor": return ["inp", "body", "bind"];
    case "constant": return ["inp", "args"];
    case "eraser": return ["inp"];
    case "duplicator": return ["inp", "outs"];
    case "delimiter": return ["inp", "out"];
    case "multiplexer": r = ["val"]; break;
    case "case": r = ["inp", "out"]; break;
    case "operator": r = ["inp"]; break;
  }
  for (let i = 0; i < node.length; i++)
    r.push(i);
  return r;
}
function primaryPort(node, leftmost) {
  switch (node.tag) {
    case "applicator": return node.func;
    case "case": return node.out;
    case "operator": return node.lmop == 0 ? node.inp : node.ops[node.lmop - 1];
    case "constant":
      if (leftmost) return null;
    default:
      return node[ports(node)[0]];
  }
}
function eqNode(n1, n2) {
  if (n1.tag !== n2.tag) return false;
  if (n1.tag === "delimiter" && n1.level === n2.level) return true;

  if (n1.tag === "multiplexer" && n1.refs.length === 0) assert(n1.level === n2.level);
  if (n1.tag === "multiplexer" && n1.level === n2.level) return true;

  if (n1.tag === "eraser") return true;
  if (n1.tag === "duplicator" && n1.level === n2.level) return true;
  return false;
}
function link(a, a_dir, b, b_dir) {
  let e = edge(a, a_dir, b, b_dir);
  b[b_dir] = a[a_dir] = e;
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
        let v = multiplexer(term.name, 0);
        link(v, "val", l, "bind");
        if (env.has(v.name)) {
          env.get(v.name).push(v);
        } else {
          env.set(v.name, [v]);
        }
        stack.push(v);
        resolve(term.body, l, "body");
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
              link(parent, parent_dir, d, "out");
              parent = d;
              parent_dir = "inp";
            }
            let l = v.length++;
            link(parent, parent_dir, v, l);
            return v;
          }
        }
        let c = constant(name);
        link(parent, parent_dir, c, "inp");
        return c;
      case "let":
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

        let e = resolve(term.expr, parent, parent_dir);

        for (let [name, _rhs] of term.binds) {
          env.get(name).pop();
        }
        return e;
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
Level = (block) => [block];
Stack = (bodyindex, levels) => { return { bodyindex, levels }; };
function readback(source, dir, stack) {
  let edge = source[dir], term, from_dir;
  if (source == edge.a && dir == edge.a_dir) {
    term = edge.b;
    from_dir = edge.b_dir;
  } else {
    term = edge.a;
    from_dir = edge.a_dir;
  }
  switch (term.tag) {
    case "applicator":
      if (from_dir == "inp") {
        return { tag: "app", func: readback(term, "func", stack), arg: readback(term, "arg", stack) };
      }
    case "abstractor": {
      if (from_dir == "inp") {
        let i = stack.bodyindex;
        let newstack = Stack(i + 1, [[Block(i + 1)]].concat(stack.levels));
        return { tag: "abs", body: readback(term, "body", newstack) };
      }
      if (from_dir == "bind") {
        return { tag: "symbol", name: stack.bodyindex - stack.levels[0][0].index };
      }
    }
    case "delimiter": {
      if (term.level == 0 && from_dir == "inp")
        return readback(term, "out", Stack(stack.bodyindex, [[Block(0)]].concat(stack.levels)));
      if (term.level == 0 && from_dir == "out")
        return readback(term, "inp", Stack(stack.bodyindex, stack.levels.slice(1)));
      let i = term.level - 1;
      // inp [ out
      let levels = Array.from(stack.levels);
      if (from_dir == "inp") {
        // σ[bκl]_i -> σ[bl,κ]_i
        let l = levels[i];
        levels.splice(i, 1, [l[0].concat(l.slice(2)), [l[1]]]);
        return readback(term, "out", Stack(stack.bodyindex, levels));
      } else if (from_dir == "out") {
        // σ[bl,κ]_i -> σ[bκl]_i
        let l = levels[i];
        levels.splice(i, 2, [l[0]].concat(levels[i + 1], l.slice(1)));
        return readback(term, "inp", Stack(stack.bodyindex, levels));
      }
      return assert(false);
    }
    case "multiplexer":
      let i = term.level;
      let levels = Array.from(stack.levels);
      if (from_dir == "val") {
        // σ[(jLδ)l]_i -> σ[jδl]_i
        let l = levels[i];
        let b = l[0];
        levels[i] = [Block(b.index, b.directors.slice(1))].concat(l.slice(1));
        return readback(term, b.directors[0], Stack(stack.bodyindex, levels));
      } else {
        // σ[jδl]_i -> σ[(jLδ)l]_i
        let l = levels[i];
        let b = l[0];
        levels[i] = [Block(b.index, [from_dir].concat(b.directors))].concat(l.slice(1));
        return readback(term, "val", Stack(stack.bodyindex, levels));
      }
  }
}

function getdot(root) {
  // traverse
  let visitedEdges = new Set();
  let visitedNodes = new Set();
  var stack = [root];
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

  var output = `digraph {\n  node [style="fill: white"]\n`;
  for (let c of visitedNodes) {
    let shape = (() => {
      switch (c.tag) {
        case "initiator": return "circle"; //"doublecircle";
        case "applicator": return "circle";
        case "abstractor": return "rect";
//        case "constant": return "star";
        case "eraser": return "circle"; //"doublecircle";
//        case "duplicator": return "fan";
//        case "delimiter": return "house";
//        case "multiplexer": return "fan";
//        case "case": return "folder";
//        case "operator": return "star";
        default: return "rect";
      }
    })();
    let human = (() => {
      switch(c.tag) {
        case "initiator": return "I";
        case "applicator": return "@";
        case "abstractor": return `λ${c.name}`;
        case "constant": return c.name;
        case "eraser": return "e";
        case "duplicator": return `/${c.level}\\`;
        case "delimiter": return `[${c.level}`;
        case "multiplexer": return `${c.name}<sub>${c.level}</sub>`;
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
    if(a_dir > arrows.length) a_dir = 0;
    if(b_dir > arrows.length) b_dir = 0;
    output += `"${a}" -> "${b}" [style="stroke: ${arrows[a_dir]}; fill: none",arrowheadStyle="fill: ${arrows[b_dir]}"];\n`;
  }
  output += `}\n`;
  return output;
}
