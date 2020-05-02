L = (n, b) => { return { tag: "abs", name: n, body: b }; };
S = (n) => { return { tag: "symbol", name: n }; };
A = (f, a) => { return { tag: "app", func: f, arg: a }; };
let l2; {
  let x = S("x"), y = S("y");
  l2 = L("x", L("y", A(x, A(x, y))));
}
t = A(l2, l2);
x = compile(t);

function evaluate(e, x) {
  if (!x || !x.tag) return x;
  switch (x.tag) {
    case "symbol": return lookup(e, x.name);
    case "combination": return monadic(null,
      () => evaluate(null, e, x.operator),
      (op) => combine(null, e, op, x.operands));
    default: return x;
  }
}
function evaluate(term, env, stack, parent, parent_dir) {
  let resolve = (t, p, pd) => evaluate(t, env, stack, p, pd);
  switch (term.tag) {
    case "abs":
      let l = abstractor(term.name);
      link(parent, parent_dir, l, "inp");
      let v = multiplexer(term.name, 0);
      link(v, "val", l, "bind");
      bind(env, name, v);
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
      for (let [name, ] of term.binds) {
        let v = multiplexer(name, 0);
        bind(env, name, v);
      }

      for (let [name, rhs] of term.binds) {
        let x = env.get(name);
        let v = x[x.length - 1];
        resolve(rhs, v, "val");
      }

      let e = resolve(term.expr, parent, parent_dir);

      for (let [name, ] of term.binds) {
        env.get(name).pop();
      }
      return e;
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
/* Environment */
make_env = (parent) => { return { bindings: Object.create(parent ? parent.bindings : null), parent } };
function lookup(e, name) {
  if (name in e.bindings) return e.bindings[name];
}
function bind(env, name, v) {
  if (env.has(name)) {
    env.get(name).push(v);
  } else {
    env.set(name, [v]);
  }
}

function compile(term) {
  let env = new Map();
  let stack = [];
  let wrapped = push_root_prompt(parse_bytecode([new Begin()].concat(boot_bytecode)));
  if (isSuspension(res)) throw "prompt not found: " + res.prompt;
  let i = initiator();
  evaluate(term, env, stack, i, "out");
  return i;
}
function combine(m, e/*env*/, cmb, o) {
  if (cmb instanceof Function) return combine(m, e, jswrap(cmb), o);
  if (!cmb || !cmb.tag) return error("not a combiner: " + to_string(cmb));
  switch (cmb.tag) {
    /* Operative & Applicative Combiners */
    case "Operative": {
      var xe = make_env(cmb.e);
      return monadic(null,
        function () { return bind(xe, cmb.p, o); },
        function () {
          return monadic(null,
            function () { return bind(xe, cmb.ep, e); },
            function () { return evaluate(null, xe, cmb.x); });
        });
    };
    case "Applicative": {
      return monadic(null,
        function () { return evalArgs(null, e, o, NIL); },
        function (args) { return combine(null, e, cmb.cmb, args); });
    };
  }
}

function evalArgs(m, e, todo, done) {
  if (todo === NIL) { return reverse_list(done); }
  return monadic(null,
    function () { return evaluate(null, e, car(todo)); },
    function (arg) {
      return evalArgs(null, e, cdr(todo), cons(arg, done));
    });
}

function bind(e, lhs, rhs) {
  if (lhs.wat_match) return lhs.wat_match(e, rhs); else return error("cannot match against: " + lhs);
}
Sym.prototype.wat_match = function (e, rhs) {
  return e.bindings[this.name] = rhs;
}
Cons.prototype.wat_match = function (e, rhs) {
  var that = this;
  return monadic(null,
    function () { return car(that).wat_match(e, car(rhs)); },
    function () { return cdr(that).wat_match(e, cdr(rhs)); });
}
Nil.prototype.wat_match = function (e, rhs) {
  if (rhs !== NIL) return error("NIL expected, but got: " + to_string(rhs));
};
Ign.prototype.wat_match = function (e, rhs) { };

/* Continuations */
function Resumption(k, f) { this.k = k; this.f = f; }
function StackFrame(fun, next, dbg, e) {
  this.fun = fun; this.next = next; this.dbg = dbg; this.e = e;
}
function isResumption(m) { return m instanceof Resumption; }
function Suspension(prompt, handler) {
  this.prompt = prompt; this.handler = handler; this.k = null;
}
function isSuspension(x) { return x instanceof Suspension; }
function suspendFrame(suspension, fun, dbg, e) {
  suspension.k = new StackFrame(fun, suspension.k, dbg, e);
}
function resumeFrame(m) {
  return m.k.fun(new Resumption(m.k.next, m.f));
}
function monadic(m, a, b) {
  if (isResumption(m)) {
    var res = resumeFrame(m);
  } else {
    var res = a();
  }
  if (isSuspension(res)) {
    suspendFrame(res, function (m) { return monadic(m, a, b); });
    return res;
  }
  return b(res);
}



function Opv(p, ep, x, e) { this.p = p; this.ep = ep; this.x = x; this.e = e; }
function Apv(cmb) { this.cmb = cmb; }
function wrap(cmb) { return new Apv(cmb); }; // type check
function unwrap(apv) { // type check
  return apv.tag === Apv ? apv.cmb : error("cannot unwrap: " + apv);
}
/* First-order Control */
function Begin() { }; function If() { }; function Loop() { }
function Catch() { }; function Finally() { }
Begin.prototype.wat_combine = function (m, e, o) {
  if (o === NIL) return null; else return begin(m, e, o);
};
function begin(m, e, xs) {
  return monadic(null,
    function () { return evaluate(null, e, car(xs)); },
    function (res) {
      var kdr = cdr(xs);
      if (kdr === NIL) return res; else return begin(null, e, kdr);
    });
}
If.prototype.wat_combine = function self(m, e, o) {
  return monadic(null,
    function () { return evaluate(null, e, elt(o, 0)); },
    function (test) {
      return evaluate(null, e, test ? elt(o, 1) : elt(o, 2));
    });
};
Loop.prototype.wat_combine = function self(m, e, o) {
  var first = true; // only resume once
  while (true) {
    if (first && isResumption(m)) {
      var res = resumeFrame(m);
    } else {
      var res = evaluate(null, e, elt(o, 0));
    }
    first = false;
    if (isSuspension(res)) {
      suspendFrame(res, function (m) { return self(m, e, o); }, elt(o, 0), e);
      return res;
    }
  }
};
Catch.prototype.wat_combine = function self(m, e, o) {
  var x = elt(o, 0);
  var handler = elt(o, 1);
  try {
    if (isResumption(m)) {
      var res = resumeFrame(m);
    } else {
      var res = evaluate(null, e, x);
    }
  } catch (exc) {
    // unwrap handler to prevent eval if exc is sym or cons
    var res = combine(null, e, unwrap(handler), list(exc));
  }
  if (isSuspension(res)) {
    suspendFrame(res, function (m) { return self(m, e, o); }, x, e);
    return res;
  } else {
    return res;
  }
};
Finally.prototype.wat_combine = function self(m, e, o) {
  var prot = elt(o, 0);
  var cleanup = elt(o, 1);
  try {
    if (isResumption(m)) {
      var res = resumeFrame(m);
    } else {
      var res = evaluate(null, e, prot);
    }
    if (isSuspension(res)) {
      suspendFrame(res, function (m) { return self(m, e, o); }, prot, e);
    }
  } finally {
    if (isSuspension(res)) {
      return res;
    } else {
      return doCleanup(null, e, cleanup, res);
    }
  }
};
function doCleanup(m, e, cleanup, res) {
  if (isResumption(m)) {
    var fres = resumeFrame(m);
  } else {
    var fres = evaluate(null, e, cleanup);
  }
  if (isSuspension(fres)) {
    suspendFrame(fres, function (m) { return doCleanup(m, e, cleanup, res); }, cleanup, e);
    return fres;
  } else {
    return res;
  }
}
/* Delimited Control */
function PushPrompt() { }; function TakeSubcont() { }; function PushSubcont() { }; function PushPromptSubcont() { }
PushPrompt.prototype.wat_combine = function self(m, e, o) {
  var prompt = elt(o, 0);
  var x = elt(o, 1);
  if (isResumption(m)) {
    var res = resumeFrame(m);
  } else {
    var res = evaluate(null, e, x);
  }
  if (isSuspension(res)) {
    if (res.prompt === prompt) {
      var continuation = res.k;
      var handler = res.handler;
      return combine(null, e, handler, cons(continuation, NIL));
    } else {
      suspendFrame(res, function (m) { return self(m, e, o); }, x, e);
      return res;
    }
  } else {
    return res;
  }
};
TakeSubcont.prototype.wat_combine = function (m, e, o) {
  var prompt = elt(o, 0);
  var handler = elt(o, 1);
  var cap = new Suspension(prompt, handler);
  suspendFrame(cap, function (m) { return combine(null, e, m.f, NIL); }, this, e);
  return cap;
};
PushSubcont.prototype.wat_combine = function self(m, e, o) {
  var thek = elt(o, 0);
  var thef = elt(o, 1);
  if (isResumption(m)) {
    var res = resumeFrame(m);
  } else {
    var res = resumeFrame(new Resumption(thek, thef));
  }
  if (isSuspension(res)) {
    suspendFrame(res, function (m) { return self(m, e, o); }, thef, e);
    return res;
  } else {
    return res;
  }
};
PushPromptSubcont.prototype.wat_combine = function self(m, e, o) {
  var prompt = elt(o, 0);
  var thek = elt(o, 1);
  var thef = elt(o, 2);
  if (isResumption(m)) {
    var res = resumeFrame(m);
  } else {
    var res = resumeFrame(new Resumption(thek, thef));
  }
  if (isSuspension(res)) {
    if (res.prompt === prompt) {
      var continuation = res.k;
      var handler = res.handler;
      return combine(null, e, handler, cons(continuation, NIL));
    } else {
      suspendFrame(res, function (m) { return self(m, e, o); }, thef, e);
      return res;
    }
  } else {
    return res;
  }
};
/* Dynamic Variables */
function DV(val) { this.val = val; }
function DNew() { }; function DRef() { }; function DLet() { }
DNew.prototype.wat_combine = function (m, e, o) { return new DV(elt(o, 0)); };
DRef.prototype.wat_combine = function (m, e, o) { return elt(o, 0).val; };
DLet.prototype.wat_combine = function self(m, e, o) {
  var dv = elt(o, 0);
  var val = elt(o, 1);
  var x = elt(o, 2);
  var oldVal = dv.val;
  dv.val = val;
  try {
    if (isResumption(m)) {
      var res = resumeFrame(m);
    } else {
      var res = evaluate(null, e, x);
    }
    if (isSuspension(res)) {
      suspendFrame(res, function (m) { return self(m, e, o); }, x, e);
      return res;
    } else {
      return res;
    }
  } finally {
    dv.val = oldVal;
  }
};
/* Forms */
function Nil() { }; var NIL = new Nil();
function Ign() { }; var IGN = new Ign();
function cons(car, cdr) { return new Cons(car, cdr); }
function car(cons) { // tc
  if (cons instanceof Cons) return cons.car; else return error("not a cons: " + to_string(cons));
}
function cdr(cons) { // tc
  if (cons instanceof Cons) return cons.cdr; else return error("not a cons: " + to_string(cons));
}
function elt(cons, i) { return (i === 0) ? car(cons) : elt(cdr(cons), i - 1); }
function sym_name(sym) { return sym.name; }
/* Setter - you are not expected to understand this - immediately */
var SETTER = jswrap(function setter(obj) { return obj.wat_setter; });
SETTER.wat_setter = jswrap(function (new_setter, obj) { obj.wat_setter = new_setter; });
/* Error handling */
var ROOT_PROMPT = {};
function push_root_prompt(x) { return list(new PushPrompt(), ROOT_PROMPT, x); }
function error(err) {
  var print_stacktrace = the_environment.bindings["user-break"];
  if (print_stacktrace !== undefined) {
    return combine(null, the_environment, print_stacktrace, list(err));
  } else { throw err; }
}
/* Utilities */
function list() {
  return array_to_list(Array.prototype.slice.call(arguments));
}
function list_star() {
  var len = arguments.length; var c = len >= 1 ? arguments[len - 1] : NIL;
  for (var i = len - 1; i > 0; i--) c = cons(arguments[i - 1], c); return c;
}
function array_to_list(array, end) {
  var c = end ? end : NIL;
  for (var i = array.length; i > 0; i--) c = cons(array[i - 1], c); return c;
}
function list_to_array(c) {
  var res = []; while (c !== NIL) { res.push(car(c)); c = cdr(c); } return res;
}
function reverse_list(list) {
  var res = NIL; while (list !== NIL) { res = cons(car(list), res); list = cdr(list); } return res;
}
/* Bytecode parser */
function parse_bytecode(obj) {
  switch (Object.prototype.toString.call(obj)) {
    case "[object String]": return obj === "#ignore" ? IGN : sym(obj);
    case "[object Array]": return parse_bytecode_array(obj);
    default: return obj;
  }
}
function parse_bytecode_array(arr) {
  if ((arr.length == 2) && arr[0] === "wat-string") { return arr[1]; }
  var i = arr.indexOf(".");
  if (i === -1) return array_to_list(arr.map(parse_bytecode));
  else {
    var front = arr.slice(0, i);
    return array_to_list(front.map(parse_bytecode), parse_bytecode(arr[i + 1]));
  }
}
/* JSNI */
var js_types = ["Array", "Boolean", "Date", "Function", "Number", "Object", "RegExp", "String"];
function is_type(obj, type_obj, type_name) {
  if (!type_obj) return error("type is undefined");
  if (js_types.indexOf(type_name) === -1) { return obj instanceof type_obj; }
  else { return toString.call(obj) === "[object " + type_name + "]"; }
}
function JSFun(jsfun) {
  if (Object.prototype.toString.call(jsfun) !== "[object Function]") return error("no fun");
  this.jsfun = jsfun;
}
JSFun.prototype.wat_combine = function (m, e, o) {
  return this.jsfun.apply(null, list_to_array(o));
};
function jswrap(jsfun) { return wrap(new JSFun(jsfun)); }
function js_unop(op) { return jswrap(new Function("a", "return (" + op + " a)")); }
function js_binop(op) { return jswrap(new Function("a", "b", "return (a " + op + " b)")); }
function js_invoker(method_name) {
  return jswrap(function () {
    if (arguments.length < 1) return error("invoker called with wrong args: " + arguments);
    if (!method_name) return error("method name is null/undefined");
    var rcv = arguments[0];
    if (!rcv) return error("receiver is null/undefined");
    var method = rcv[method_name];
    if (!method) return error("method not found: " + method_name + " in: " + to_string(rcv));
    return method.apply(rcv, Array.prototype.slice.call(arguments, 1));
  });
}
function js_getter(prop_name) {
  var getter = jswrap(function () {
    if (arguments.length !== 1) return error(prop_name + " getter called with wrong args");
    var rcv = arguments[0];
    if ((rcv !== undefined) && (rcv !== null)) return rcv[prop_name];
    else return error("can't get " + prop_name + " of " + rcv);
  });
  getter.wat_setter = js_setter(prop_name); return getter;
}
function js_setter(prop_name) {
  return jswrap(function () {
    if (arguments.length !== 2) return error("setter called with wrong args: " + arguments);
    var rcv = arguments[1];
    if ((rcv !== undefined) && (rcv !== null)) return rcv[prop_name] = arguments[0];
    else return error("can't set " + prop_name + " of " + rcv);
  });
}
function make_prototype(name) {
  var prop_names = Array.prototype.slice.call(arguments, 1);
  var param_names = prop_names.join(",");
  var param_inits = prop_names.map(function (prop_name) {
    return "this." + prop_name + "=" + prop_name + ";";
  }).join("");
  return eval("(function " + name + "(" + param_names + "){" + param_inits + "})");
}
function jsnew(ctor) {
  var factoryFunction = ctor.bind.apply(ctor, arguments);
  return new factoryFunction();
}
function js_function(cmb) {
  return function () {
    var args = cons(this, array_to_list(Array.prototype.slice.call(arguments)));
    return combine(null, null, cmb, args);
  }
}
var JS_GLOBAL = jswrap(function (name) { return eval(name); });
JS_GLOBAL.wat_setter = jswrap(function (new_val, name) { global[name] = new_val; });
/* Stringification */
function to_string(obj) {
  if (toString.call(obj) === "[object String]") return JSON.stringify(obj);
  else if ((obj !== null) && (obj !== undefined)) return obj.toString();
  else return Object.prototype.toString.call(obj);
}
Nil.prototype.toString = function () { return "()"; };
Ign.prototype.toString = function () { return "#ignore"; };
Sym.prototype.toString = function () { return this.name; };
Cons.prototype.toString = function () { return "(" + cons_to_string(this) + ")" };
function cons_to_string(c) {
  if (cdr(c) === NIL) return to_string(car(c));
  else if (cdr(c) instanceof Cons) { return to_string(car(c)) + " " + cons_to_string(cdr(c)); }
  else return to_string(car(c)) + " . " + to_string(cdr(c));
}
Apv.prototype.toString = function () { return "[Apv " + to_string(this.cmb) + "]"; };
Opv.prototype.toString = function () {
  return "[Opv " + to_string(this.p) + " " + to_string(this.ep) + " " + to_string(this.x) + "]";
};
Vau.prototype.toString = function () { return "vm-vau"; };
Def.prototype.toString = function () { return "vm-def"; };
Eval.prototype.toString = function () { return "vm-eval"; };
Begin.prototype.toString = function () { return "vm-begin"; };
If.prototype.toString = function () { return "vm-if"; };
Loop.prototype.toString = function () { return "vm-loop"; };
Catch.prototype.toString = function () { return "vm-catch"; };
Finally.prototype.toString = function () { return "vm-finally"; };
DLet.prototype.toString = function () { return "vm-dlet"; }
DNew.prototype.toString = function () { return "vm-dnew"; }
DRef.prototype.toString = function () { return "vm-dref"; }
PushPrompt.prototype.toString = function () { return "vm-push-prompt"; }
TakeSubcont.prototype.toString = function () { return "vm-take-subcont"; }
PushSubcont.prototype.toString = function () { return "vm-push-subcont"; }
PushPromptSubcont.prototype.toString = function () { return "vm-push-prompt-subcont"; }
JSFun.prototype.toString = function () { return "[JSFun " + this.jsfun.toString() + "]"; };
/* Bootstrap */
var builtin_bytecode =
  ["vm-begin",
    // Basics
    ["vm-def", "vm-vau", new Vau()],
    ["vm-def", "vm-eval", wrap(new Eval())],
    ["vm-def", "vm-make-environment", jswrap(function (env) { return make_env(env); })],
    ["vm-def", "vm-wrap", jswrap(wrap)],
    ["vm-def", "vm-unwrap", jswrap(unwrap)],
    // Values
    ["vm-def", "vm-cons", jswrap(cons)],
    ["vm-def", "vm-cons?", jswrap(function (obj) { return obj instanceof Cons; })],
    ["vm-def", "vm-nil?", jswrap(function (obj) { return obj === NIL; })],
    ["vm-def", "vm-string-to-symbol", jswrap(sym)],
    ["vm-def", "vm-symbol?", jswrap(function (obj) { return obj instanceof Sym; })],
    ["vm-def", "vm-symbol-name", jswrap(sym_name)],
    // First-order Control
    ["vm-def", "vm-if", new If()],
    ["vm-def", "vm-loop", new Loop()],
    ["vm-def", "vm-throw", jswrap(function (err) { throw err; })],
    ["vm-def", "vm-catch", new Catch()],
    ["vm-def", "vm-finally", new Finally()],
    // Delimited Control
    ["vm-def", "vm-push-prompt", new PushPrompt()],
    ["vm-def", "vm-take-subcont", wrap(new TakeSubcont())],
    ["vm-def", "vm-push-subcont", wrap(new PushSubcont())],
    ["vm-def", "vm-push-prompt-subcont", wrap(new PushPromptSubcont())],
    // Dynamically-scoped Variables
    ["vm-def", "vm-dnew", wrap(new DNew())],
    ["vm-def", "vm-dlet", new DLet()],
    ["vm-def", "vm-dref", wrap(new DRef())],
    // Setters
    ["vm-def", "vm-setter", SETTER],
    // Errors
    ["vm-def", "vm-root-prompt", ROOT_PROMPT],
    ["vm-def", "vm-error", jswrap(error)],
    // JS Interface
    ["vm-def", "vm-js-wrap", jswrap(jswrap)],
    ["vm-def", "vm-js-unop", jswrap(js_unop)],
    ["vm-def", "vm-js-binop", jswrap(js_binop)],
    ["vm-def", "vm-js-getter", jswrap(js_getter)],
    ["vm-def", "vm-js-setter", jswrap(js_setter)],
    ["vm-def", "vm-js-invoker", jswrap(js_invoker)],
    ["vm-def", "vm-js-function", jswrap(js_function)],
    ["vm-def", "vm-js-global", JS_GLOBAL],
    ["vm-def", "vm-js-make-object", jswrap(function () { return {}; })],
    ["vm-def", "vm-js-make-prototype", jswrap(make_prototype)],
    ["vm-def", "vm-js-new", jswrap(jsnew)],
    ["vm-def", "vm-type?", jswrap(is_type)],
    // Utilities
    ["vm-def", "vm-list-to-array", jswrap(list_to_array)],
    ["vm-def", "vm-array-to-list", jswrap(array_to_list)],
    ["vm-def", "vm-reverse-list", jswrap(reverse_list)],
    ["vm-def", "vm-list*", jswrap(list_star)]
  ];


function evaluate(e, x) {
  if (!x || !x.tag) return x;
  switch (x.tag) {
    case "symbol": return lookup(e, x.name);
    case "combination": return monadic(null,
      () => evaluate(null, e, x.operator),
      (op) => combine(null, e, op, x.operands));
    default: return x;
  }
}

function combine(m, e/*env*/, cmb, o) {
  if (cmb instanceof Function) return combine(m, e, jswrap(cmb), o);
  if (!cmb || !cmb.tag) return error("not a combiner: " + to_string(cmb));
  switch (cmb.tag) {
    /* Operative & Applicative Combiners */
    case "Operative": {
      var xe = make_env(cmb.e);
      return monadic(null,
        function () { return bind(xe, cmb.p, o); },
        function () {
          return monadic(null,
            function () { return bind(xe, cmb.ep, e); },
            function () { return evaluate(null, xe, cmb.x); });
        });
    };
    case "Applicative": {
      return monadic(null,
        function () { return evalArgs(null, e, o, NIL); },
        function (args) { return combine(null, e, cmb.cmb, args); });
    };
  }
}

function evalArgs(m, e, todo, done) {
  if (todo === NIL) { return reverse_list(done); }
  return monadic(null,
    function () { return evaluate(null, e, car(todo)); },
    function (arg) {
      return evalArgs(null, e, cdr(todo), cons(arg, done));
    });
}

var wrapped = push_root_prompt(parse_bytecode([new Begin()].concat(boot_bytecode)));
var res = evaluate(null, the_environment, wrapped);
if (isSuspension(res)) throw "prompt not found: " + res.prompt;

/* Continuations */
function Resumption(k, f) { this.k = k; this.f = f; }
function StackFrame(fun, next, dbg, e) {
  this.fun = fun; this.next = next; this.dbg = dbg; this.e = e;
}
function isResumption(m) { return m instanceof Resumption; }
function Suspension(prompt, handler) {
  this.prompt = prompt; this.handler = handler; this.k = null;
}
function isSuspension(x) { return x instanceof Suspension; }
function suspendFrame(suspension, fun, dbg, e) {
  suspension.k = new StackFrame(fun, suspension.k, dbg, e);
}
function resumeFrame(m) {
  return m.k.fun(new Resumption(m.k.next, m.f));
}
function monadic(m, a, b) {
  if (isResumption(m)) {
    var res = resumeFrame(m);
  } else {
    var res = a();
  }
  if (isSuspension(res)) {
    suspendFrame(res, function (m) { return monadic(m, a, b); });
    return res;
  }
  return b(res);
}



function Opv(p, ep, x, e) { this.p = p; this.ep = ep; this.x = x; this.e = e; }
function Apv(cmb) { this.cmb = cmb; }
function wrap(cmb) { return new Apv(cmb); }; // type check
function unwrap(apv) { // type check
  return apv.tag === Apv ? apv.cmb : error("cannot unwrap: " + apv);
}
/* First-order Control */
function Begin() { }; function If() { }; function Loop() { }
function Catch() { }; function Finally() { }
Begin.prototype.wat_combine = function (m, e, o) {
  if (o === NIL) return null; else return begin(m, e, o);
};
function begin(m, e, xs) {
  return monadic(null,
    function () { return evaluate(null, e, car(xs)); },
    function (res) {
      var kdr = cdr(xs);
      if (kdr === NIL) return res; else return begin(null, e, kdr);
    });
}
If.prototype.wat_combine = function self(m, e, o) {
  return monadic(null,
    function () { return evaluate(null, e, elt(o, 0)); },
    function (test) {
      return evaluate(null, e, test ? elt(o, 1) : elt(o, 2));
    });
};
Loop.prototype.wat_combine = function self(m, e, o) {
  var first = true; // only resume once
  while (true) {
    if (first && isResumption(m)) {
      var res = resumeFrame(m);
    } else {
      var res = evaluate(null, e, elt(o, 0));
    }
    first = false;
    if (isSuspension(res)) {
      suspendFrame(res, function (m) { return self(m, e, o); }, elt(o, 0), e);
      return res;
    }
  }
};
Catch.prototype.wat_combine = function self(m, e, o) {
  var x = elt(o, 0);
  var handler = elt(o, 1);
  try {
    if (isResumption(m)) {
      var res = resumeFrame(m);
    } else {
      var res = evaluate(null, e, x);
    }
  } catch (exc) {
    // unwrap handler to prevent eval if exc is sym or cons
    var res = combine(null, e, unwrap(handler), list(exc));
  }
  if (isSuspension(res)) {
    suspendFrame(res, function (m) { return self(m, e, o); }, x, e);
    return res;
  } else {
    return res;
  }
};
Finally.prototype.wat_combine = function self(m, e, o) {
  var prot = elt(o, 0);
  var cleanup = elt(o, 1);
  try {
    if (isResumption(m)) {
      var res = resumeFrame(m);
    } else {
      var res = evaluate(null, e, prot);
    }
    if (isSuspension(res)) {
      suspendFrame(res, function (m) { return self(m, e, o); }, prot, e);
    }
  } finally {
    if (isSuspension(res)) {
      return res;
    } else {
      return doCleanup(null, e, cleanup, res);
    }
  }
};
function doCleanup(m, e, cleanup, res) {
  if (isResumption(m)) {
    var fres = resumeFrame(m);
  } else {
    var fres = evaluate(null, e, cleanup);
  }
  if (isSuspension(fres)) {
    suspendFrame(fres, function (m) { return doCleanup(m, e, cleanup, res); }, cleanup, e);
    return fres;
  } else {
    return res;
  }
}
/* Delimited Control */
function PushPrompt() { }; function TakeSubcont() { }; function PushSubcont() { }; function PushPromptSubcont() { }
PushPrompt.prototype.wat_combine = function self(m, e, o) {
  var prompt = elt(o, 0);
  var x = elt(o, 1);
  if (isResumption(m)) {
    var res = resumeFrame(m);
  } else {
    var res = evaluate(null, e, x);
  }
  if (isSuspension(res)) {
    if (res.prompt === prompt) {
      var continuation = res.k;
      var handler = res.handler;
      return combine(null, e, handler, cons(continuation, NIL));
    } else {
      suspendFrame(res, function (m) { return self(m, e, o); }, x, e);
      return res;
    }
  } else {
    return res;
  }
};
TakeSubcont.prototype.wat_combine = function (m, e, o) {
  var prompt = elt(o, 0);
  var handler = elt(o, 1);
  var cap = new Suspension(prompt, handler);
  suspendFrame(cap, function (m) { return combine(null, e, m.f, NIL); }, this, e);
  return cap;
};
PushSubcont.prototype.wat_combine = function self(m, e, o) {
  var thek = elt(o, 0);
  var thef = elt(o, 1);
  if (isResumption(m)) {
    var res = resumeFrame(m);
  } else {
    var res = resumeFrame(new Resumption(thek, thef));
  }
  if (isSuspension(res)) {
    suspendFrame(res, function (m) { return self(m, e, o); }, thef, e);
    return res;
  } else {
    return res;
  }
};
PushPromptSubcont.prototype.wat_combine = function self(m, e, o) {
  var prompt = elt(o, 0);
  var thek = elt(o, 1);
  var thef = elt(o, 2);
  if (isResumption(m)) {
    var res = resumeFrame(m);
  } else {
    var res = resumeFrame(new Resumption(thek, thef));
  }
  if (isSuspension(res)) {
    if (res.prompt === prompt) {
      var continuation = res.k;
      var handler = res.handler;
      return combine(null, e, handler, cons(continuation, NIL));
    } else {
      suspendFrame(res, function (m) { return self(m, e, o); }, thef, e);
      return res;
    }
  } else {
    return res;
  }
};
/* Dynamic Variables */
function DV(val) { this.val = val; }
function DNew() { }; function DRef() { }; function DLet() { }
DNew.prototype.wat_combine = function (m, e, o) { return new DV(elt(o, 0)); };
DRef.prototype.wat_combine = function (m, e, o) { return elt(o, 0).val; };
DLet.prototype.wat_combine = function self(m, e, o) {
  var dv = elt(o, 0);
  var val = elt(o, 1);
  var x = elt(o, 2);
  var oldVal = dv.val;
  dv.val = val;
  try {
    if (isResumption(m)) {
      var res = resumeFrame(m);
    } else {
      var res = evaluate(null, e, x);
    }
    if (isSuspension(res)) {
      suspendFrame(res, function (m) { return self(m, e, o); }, x, e);
      return res;
    } else {
      return res;
    }
  } finally {
    dv.val = oldVal;
  }
};
/* Forms */
function Nil() { }; var NIL = new Nil();
function Ign() { }; var IGN = new Ign();
function cons(car, cdr) { return new Cons(car, cdr); }
function car(cons) { // tc
  if (cons instanceof Cons) return cons.car; else return error("not a cons: " + to_string(cons));
}
function cdr(cons) { // tc
  if (cons instanceof Cons) return cons.cdr; else return error("not a cons: " + to_string(cons));
}
function elt(cons, i) { return (i === 0) ? car(cons) : elt(cdr(cons), i - 1); }
function sym_name(sym) { return sym.name; }
/* Environment */
function Env(parent) { this.bindings = Object.create(parent ? parent.bindings : null); this.parent = parent; }
function make_env(parent) { return new Env(parent); }
function lookup(e, name) {
  if (name in e.bindings) return e.bindings[name];
  else return error("unbound: " + name);
}
function bind(e, lhs, rhs) {
  if (lhs.wat_match) return lhs.wat_match(e, rhs); else return error("cannot match against: " + lhs);
}
Sym.prototype.wat_match = function (e, rhs) {
  return e.bindings[this.name] = rhs;
}
Cons.prototype.wat_match = function (e, rhs) {
  var that = this;
  return monadic(null,
    function () { return car(that).wat_match(e, car(rhs)); },
    function () { return cdr(that).wat_match(e, cdr(rhs)); });
}
Nil.prototype.wat_match = function (e, rhs) {
  if (rhs !== NIL) return error("NIL expected, but got: " + to_string(rhs));
};
Ign.prototype.wat_match = function (e, rhs) { };
/* Setter - you are not expected to understand this - immediately */
var SETTER = jswrap(function setter(obj) { return obj.wat_setter; });
SETTER.wat_setter = jswrap(function (new_setter, obj) { obj.wat_setter = new_setter; });
/* Error handling */
var ROOT_PROMPT = {};
function push_root_prompt(x) { return list(new PushPrompt(), ROOT_PROMPT, x); }
function error(err) {
  var print_stacktrace = the_environment.bindings["user-break"];
  if (print_stacktrace !== undefined) {
    return combine(null, the_environment, print_stacktrace, list(err));
  } else { throw err; }
}
/* Utilities */
function list() {
  return array_to_list(Array.prototype.slice.call(arguments));
}
function list_star() {
  var len = arguments.length; var c = len >= 1 ? arguments[len - 1] : NIL;
  for (var i = len - 1; i > 0; i--) c = cons(arguments[i - 1], c); return c;
}
function array_to_list(array, end) {
  var c = end ? end : NIL;
  for (var i = array.length; i > 0; i--) c = cons(array[i - 1], c); return c;
}
function list_to_array(c) {
  var res = []; while (c !== NIL) { res.push(car(c)); c = cdr(c); } return res;
}
function reverse_list(list) {
  var res = NIL; while (list !== NIL) { res = cons(car(list), res); list = cdr(list); } return res;
}
/* Bytecode parser */
function parse_bytecode(obj) {
  switch (Object.prototype.toString.call(obj)) {
    case "[object String]": return obj === "#ignore" ? IGN : sym(obj);
    case "[object Array]": return parse_bytecode_array(obj);
    default: return obj;
  }
}
function parse_bytecode_array(arr) {
  if ((arr.length == 2) && arr[0] === "wat-string") { return arr[1]; }
  var i = arr.indexOf(".");
  if (i === -1) return array_to_list(arr.map(parse_bytecode));
  else {
    var front = arr.slice(0, i);
    return array_to_list(front.map(parse_bytecode), parse_bytecode(arr[i + 1]));
  }
}
/* JSNI */
var js_types = ["Array", "Boolean", "Date", "Function", "Number", "Object", "RegExp", "String"];
function is_type(obj, type_obj, type_name) {
  if (!type_obj) return error("type is undefined");
  if (js_types.indexOf(type_name) === -1) { return obj instanceof type_obj; }
  else { return toString.call(obj) === "[object " + type_name + "]"; }
}
function JSFun(jsfun) {
  if (Object.prototype.toString.call(jsfun) !== "[object Function]") return error("no fun");
  this.jsfun = jsfun;
}
JSFun.prototype.wat_combine = function (m, e, o) {
  return this.jsfun.apply(null, list_to_array(o));
};
function jswrap(jsfun) { return wrap(new JSFun(jsfun)); }
function js_unop(op) { return jswrap(new Function("a", "return (" + op + " a)")); }
function js_binop(op) { return jswrap(new Function("a", "b", "return (a " + op + " b)")); }
function js_invoker(method_name) {
  return jswrap(function () {
    if (arguments.length < 1) return error("invoker called with wrong args: " + arguments);
    if (!method_name) return error("method name is null/undefined");
    var rcv = arguments[0];
    if (!rcv) return error("receiver is null/undefined");
    var method = rcv[method_name];
    if (!method) return error("method not found: " + method_name + " in: " + to_string(rcv));
    return method.apply(rcv, Array.prototype.slice.call(arguments, 1));
  });
}
function js_getter(prop_name) {
  var getter = jswrap(function () {
    if (arguments.length !== 1) return error(prop_name + " getter called with wrong args");
    var rcv = arguments[0];
    if ((rcv !== undefined) && (rcv !== null)) return rcv[prop_name];
    else return error("can't get " + prop_name + " of " + rcv);
  });
  getter.wat_setter = js_setter(prop_name); return getter;
}
function js_setter(prop_name) {
  return jswrap(function () {
    if (arguments.length !== 2) return error("setter called with wrong args: " + arguments);
    var rcv = arguments[1];
    if ((rcv !== undefined) && (rcv !== null)) return rcv[prop_name] = arguments[0];
    else return error("can't set " + prop_name + " of " + rcv);
  });
}
function make_prototype(name) {
  var prop_names = Array.prototype.slice.call(arguments, 1);
  var param_names = prop_names.join(",");
  var param_inits = prop_names.map(function (prop_name) {
    return "this." + prop_name + "=" + prop_name + ";";
  }).join("");
  return eval("(function " + name + "(" + param_names + "){" + param_inits + "})");
}
function jsnew(ctor) {
  var factoryFunction = ctor.bind.apply(ctor, arguments);
  return new factoryFunction();
}
function js_function(cmb) {
  return function () {
    var args = cons(this, array_to_list(Array.prototype.slice.call(arguments)));
    return combine(null, null, cmb, args);
  }
}
var JS_GLOBAL = jswrap(function (name) { return eval(name); });
JS_GLOBAL.wat_setter = jswrap(function (new_val, name) { global[name] = new_val; });
/* Stringification */
function to_string(obj) {
  if (toString.call(obj) === "[object String]") return JSON.stringify(obj);
  else if ((obj !== null) && (obj !== undefined)) return obj.toString();
  else return Object.prototype.toString.call(obj);
}
Nil.prototype.toString = function () { return "()"; };
Ign.prototype.toString = function () { return "#ignore"; };
Sym.prototype.toString = function () { return this.name; };
Cons.prototype.toString = function () { return "(" + cons_to_string(this) + ")" };
function cons_to_string(c) {
  if (cdr(c) === NIL) return to_string(car(c));
  else if (cdr(c) instanceof Cons) { return to_string(car(c)) + " " + cons_to_string(cdr(c)); }
  else return to_string(car(c)) + " . " + to_string(cdr(c));
}
Apv.prototype.toString = function () { return "[Apv " + to_string(this.cmb) + "]"; };
Opv.prototype.toString = function () {
  return "[Opv " + to_string(this.p) + " " + to_string(this.ep) + " " + to_string(this.x) + "]";
};
Vau.prototype.toString = function () { return "vm-vau"; };
Def.prototype.toString = function () { return "vm-def"; };
Eval.prototype.toString = function () { return "vm-eval"; };
Begin.prototype.toString = function () { return "vm-begin"; };
If.prototype.toString = function () { return "vm-if"; };
Loop.prototype.toString = function () { return "vm-loop"; };
Catch.prototype.toString = function () { return "vm-catch"; };
Finally.prototype.toString = function () { return "vm-finally"; };
DLet.prototype.toString = function () { return "vm-dlet"; }
DNew.prototype.toString = function () { return "vm-dnew"; }
DRef.prototype.toString = function () { return "vm-dref"; }
PushPrompt.prototype.toString = function () { return "vm-push-prompt"; }
TakeSubcont.prototype.toString = function () { return "vm-take-subcont"; }
PushSubcont.prototype.toString = function () { return "vm-push-subcont"; }
PushPromptSubcont.prototype.toString = function () { return "vm-push-prompt-subcont"; }
JSFun.prototype.toString = function () { return "[JSFun " + this.jsfun.toString() + "]"; };
/* Bootstrap */
var builtin_bytecode =
  ["vm-begin",
    // Basics
    ["vm-def", "vm-vau", new Vau()],
    ["vm-def", "vm-eval", wrap(new Eval())],
    ["vm-def", "vm-make-environment", jswrap(function (env) { return make_env(env); })],
    ["vm-def", "vm-wrap", jswrap(wrap)],
    ["vm-def", "vm-unwrap", jswrap(unwrap)],
    // Values
    ["vm-def", "vm-cons", jswrap(cons)],
    ["vm-def", "vm-cons?", jswrap(function (obj) { return obj instanceof Cons; })],
    ["vm-def", "vm-nil?", jswrap(function (obj) { return obj === NIL; })],
    ["vm-def", "vm-string-to-symbol", jswrap(sym)],
    ["vm-def", "vm-symbol?", jswrap(function (obj) { return obj instanceof Sym; })],
    ["vm-def", "vm-symbol-name", jswrap(sym_name)],
    // First-order Control
    ["vm-def", "vm-if", new If()],
    ["vm-def", "vm-loop", new Loop()],
    ["vm-def", "vm-throw", jswrap(function (err) { throw err; })],
    ["vm-def", "vm-catch", new Catch()],
    ["vm-def", "vm-finally", new Finally()],
    // Delimited Control
    ["vm-def", "vm-push-prompt", new PushPrompt()],
    ["vm-def", "vm-take-subcont", wrap(new TakeSubcont())],
    ["vm-def", "vm-push-subcont", wrap(new PushSubcont())],
    ["vm-def", "vm-push-prompt-subcont", wrap(new PushPromptSubcont())],
    // Dynamically-scoped Variables
    ["vm-def", "vm-dnew", wrap(new DNew())],
    ["vm-def", "vm-dlet", new DLet()],
    ["vm-def", "vm-dref", wrap(new DRef())],
    // Setters
    ["vm-def", "vm-setter", SETTER],
    // Errors
    ["vm-def", "vm-root-prompt", ROOT_PROMPT],
    ["vm-def", "vm-error", jswrap(error)],
    // JS Interface
    ["vm-def", "vm-js-wrap", jswrap(jswrap)],
    ["vm-def", "vm-js-unop", jswrap(js_unop)],
    ["vm-def", "vm-js-binop", jswrap(js_binop)],
    ["vm-def", "vm-js-getter", jswrap(js_getter)],
    ["vm-def", "vm-js-setter", jswrap(js_setter)],
    ["vm-def", "vm-js-invoker", jswrap(js_invoker)],
    ["vm-def", "vm-js-function", jswrap(js_function)],
    ["vm-def", "vm-js-global", JS_GLOBAL],
    ["vm-def", "vm-js-make-object", jswrap(function () { return {}; })],
    ["vm-def", "vm-js-make-prototype", jswrap(make_prototype)],
    ["vm-def", "vm-js-new", jswrap(jsnew)],
    ["vm-def", "vm-type?", jswrap(is_type)],
    // Utilities
    ["vm-def", "vm-list-to-array", jswrap(list_to_array)],
    ["vm-def", "vm-array-to-list", jswrap(array_to_list)],
    ["vm-def", "vm-reverse-list", jswrap(reverse_list)],
    ["vm-def", "vm-list*", jswrap(list_star)]
  ];
