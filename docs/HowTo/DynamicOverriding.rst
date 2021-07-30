Dynamically override functions
##############################

Here we are implementing a model where a function can be static (entirely determined at compile-time) or dynamic (possibly determined by run-time code).
Dynamic functions can be changed at run time and can call any other functions, static or dynamic.

The main feature is a macro dynamicF. dynamicF adds an implicit high-priority clause to the function:

::

  dynamicF description

  -- expands to

  prio high
  description T | Just X <- overrides ["description",T] = X.

The high priority is because dynamic changes take precedence over static conditions. For example with "occupied kitchen := true", it's an excellent guess that this should override a static check that goes through the list of people.

Then later in the code we can write

::

  description lamp := say "It's a burned-out lamp."

  -- expands to

  set-override ["description", "lamp"] (say ...)

``:=`` is an fexpr, because otherwise ``description lamp`` would be evaluated and ``:=`` wouldn't have enough information.

There's also a delete operation, which returns to the static code.

::

  delete (description "lamp")

Some examples:

::

  dynamicF 7
  7 := 8

  dynamicF double
  double n = 2 * n

  double 5 := 11

The implementation of overrides / set-override is a hash table of overridden values, with some voodoo to allow pattern overrides as well as literals. Dynamic always overrides static but we could also allow defining priorities for dynamic functions.

Detecting static
================

Dynamic functions are great, but can we get rid of the macro? Well, we could make a macro that makes all the functions in a module dynamic.

But maybe some functions will never be overridden. Hopefully, with a whole-program analysis Stroscot can detect that the hash table lookup will never return an element, and optimize the check out. But the use-case is interactive fiction, where performance is not really a big concern.

Example
=======

::

  price (obj : Treasure) = 10

  hit (obj : MingVase) =
    print "it cracks"
    price obj := 0

  main =
    mv = MingVase
    print (price mv)
    hit mv
    print (price mv)

This is defining a rule at runtime to override the static rule. := is an fexpr; otherwise price obj would be resolved to 10. Similarly all the price calls are dynamic. So translated this looks like:

::

  static_price (obj : Treasure) = 10

  hit (obj : MingVase) =
    print "it cracks"
    set_dynamic "price" obj 0

  price obj | has_dynamic state "price" obj = run_dynamic state "price" obj
            | otherwise = static_price obj

  main =
    state = ...
    mv = MingVase
    print (price mv)
    hit mv
    print (price mv)

With a uniform translation everything will be wrapped in this dynamic override. Since dynamic overrides static, nothing from static needs to carry over - the rule system can be completely different. In particular the dynamic system does not need to implement priorities, method combination, or specificity overriding. (Although, it could, with enough work).
