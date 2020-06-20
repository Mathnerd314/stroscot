FAQ
---

Does it run?
~~~~~~~~~~~~

No, it's in the design stage.

What language is the compiler written in?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A near-term goal is to write Stroscot in itself. However, it has to generate code first. I picked JavaScript to start for a number of reasons:

* It's the fastest interpreted language available
* It has reasonably up-to-date syntax and features thanks to TC39
* A lot of the :ref:`inspiring projects <inspiring-projects>` were written in JS
* LLVM compiles to JS and there are LLVM bindings available for JS
* TypeScript doesn't add much besides compilation overhead

Since then, development has shifted to Haskell, for other reasons:

* The compiler/type system prevents a lot of common errors (particularly typos, which JS doesn't detect until late)
* A lot of other type-system-heavy projects are written in Haskell

Where can I ask questions?
~~~~~~~~~~~~~~~~~~~~~~~~~~

Our `reddit community <https://www.reddit.com/r/stroscot>`__ (checked
monthly) or on the `Stroscot
Discord server <https://discord.gg/rQvE5Yj>`__.

How do I contribute?
~~~~~~~~~~~~~~~~~~~~

Any and all comments will be taken into consideration. Commit access is
available for anyone with 2 or more commits to the project.
