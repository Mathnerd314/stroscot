FAQ
---

Does it run?
~~~~~~~~~~~~

No, it's in the design stage. But there are Haskell experiments in the ``src/`` and ``test/`` directories that can be run.

What language is the compiler written in?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A near-term goal is to write Stroscot in itself. However, it has to generate code first. I originally picked JavaScript to start for a number of reasons:

* It's the fastest interpreted language available
* It has reasonably up-to-date syntax and features thanks to TC39
* A lot of the :ref:`inspiring projects <inspiring-projects>` were written in JS
* LLVM compiles to JS and there are LLVM bindings available for JS
* TypeScript doesn't add much besides compilation overhead

Since then, development has shifted to Haskell, for other reasons:

* The compiler/type system prevents a lot of common errors (particularly typos, which JS doesn't detect until late)
* A lot of other type-system-heavy projects are written in Haskell
* I'm most familiar with Haskell.

Where can I ask questions?
~~~~~~~~~~~~~~~~~~~~~~~~~~

See the :ref:`links <links>`

How do I contribute?
~~~~~~~~~~~~~~~~~~~~

See CONTRIBUTING.rst in the top-level folder of the Git repository.
