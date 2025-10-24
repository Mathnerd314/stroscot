function_declaration =
{
  formal_params: ["w", "x", "y", "z"],
  implicit_params: ["foo"],
  defaults: new Map([["x",3]]),
  outputs: ["a"]
};
env =
{
  bindings: [
    {key: "y", value: 2},
    {positional: true, value: 1}
  ]
}

function bind(declaration, env) {
  let { formal_params, implicit_params, defaults, outputs } = declaration;
  let slots = new Map();
  for (let p of formal_params) {
    slots.set(p, 'missing');
  }
  for (let p of implicit_params) {
    slots.set(p, 'missing');
  }
  for (let [p,d] of defaults) {
    assert(slots.has(p) && slots.get(p) == 'missing');
    slots.set(p,d);
  }
  for (let binding of env.bindings) {
    if (binding.key && binding.key in slots) {
      // assert(slots.get(binding.key) == 'missing'); // not true for default args
      slots.set(binding.key, binding.value);
      continue;
    }
    if (binding.positional) {
      // fill first one
      let bound = false;
      for (let [p, v] of slots) {
        if (v == 'missing') {
          slots.set(p, binding.value);
          bound = true;
          break;
        }
      }
      assert(bound); // todo: currying
    }
  }
}
