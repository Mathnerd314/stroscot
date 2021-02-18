Compiler design
###############

Pipeline
========

The start is a parser - this will be written later once partial evaluation is sufficient to specialize naive parsers efficiently. For now the code is input using ADTs and parentheses. The parser will also add token start/end and other debugging information.

Next is the fexpr interpreter loop. This starts with the ADT tree and produces evaluated code. Parts of the evaluator include turning name-strings into direct evaluation graph references and compiling pattern matching to tag scrutinization.

Currying is handled by a pass that creates partially-applied functions using the eval-apply model, similar to :cite:`downenMakingFasterCurry2019`. Initially all user code starts out using one-argument functions.

Currently there are no code targets implemented - the main interactive element is an interpreter. There are some papers on partial evaluation and supercompilation that will probably get used for a C backend or a JIT or something.

Flags
=====

In general flags can take 4 levels: ignore, warn, error, and fix. Ignore ignores the issue as much as possible. Warn issues a warning but otherwise ignores the issue. Error stops the compiler from continuing. Fix automatically constructs a fix for the issue and modifies the source file(s) in-place.

Error messages
==============

Since Stroscot uses model checking, most failures will end up producing a counterexample. The counterexample may not be minimal, but it is much easier to debug a concrete instance than to try to figure one out from contextual information.

For source locations we produce the start/end span of two (filename, line number, column number) tuples. Go uses an efficient memory-map-like model from these tuples to integers, to avoid passing around strings. We could just use pointers, but Go's integers are ordered so comparing within files is faster. But it isn't clear how to make this incremental.

Output
======

The simplest output is to write out a copy of the interpreter with a comment "This is generated code - see Interpreter.hs for source" at the top. But we want to specialize this.

versioning of time/date
identifier minimization/translation
unit test
random input testing
quasiquotation
typechecking

Cross compilation
=================

In cross compilation we have not one system, but two systems. To use the newer `Clang <https://clang.llvm.org/docs/CrossCompilation.html>`__ terminology, there is the **host** system where the program is being built, and the **target** system where the program will run. When the host and target systems are the same, it's a native build; otherwise it's a cross build.

The older `GNU terminology <https://gcc.gnu.org/onlinedocs/gccint/Configure-Terms.html>`__ uses a triple, build/host/target; but the "target" there is really a configuration option, namely the supported target of the compiler that will run on the host. Programs that aren't compilers need only build/host specified, but remembering whether the build system builds the host or vice-versa is tricky. Overall the Clang terminology host/target/supported targets seems clearer than build/host/target.

Bootstrapping
=============

Bootstrapping is a 2-stage process. We start with the source ``s`` and bootstrap compiler ``cB``, an old compiler using the old ABI. Then we build:

* stage 1: New compiler on old ABI ``c1=compile(s,cB)``
* stage 2: New compiler on new ABI ``c2=compile(s,c1)``

We can test stage 2 (the "compiler bootstrap test") by building a new compiler ``c3=compile(s,c2)``. If the build is deterministic, ``c3`` should be bit-identical to ``c2``. We can also run the test suite to compare outputs of ``c1`` and ``c2``. But we cannot compare performance of ``c1`` and ``c2``, because they use different ABIs, and also ``cB`` may be buggy so ``c1`` and ``c2`` may not behave exactly the same. We can also use diverse double-compiling :cite:`wheelerFullyCounteringTrusting2010`, compiling with multiple bootstrap compilers ``cB``, to increase our confidence in the correctness of the stage 2 compiler.

For cross-compiling, we build stage 1 for the host and stage 2 for the target.

Actually this is more complex. The compiler is really two components, an interpreter and a specializer; in a full system there are many components:
* source-level libraries (macros) ``m``
* bytecode libraries (plugins) ``b``
* compiled libraries / run-time components (e.g. libc, GC) ``r``
* the interpreter ``i``
* specializer ``s``

A primitive ``run : program -> data -> data``, that can run programs written in any language, given input data, and produce an output result. We use an oracle notion of result where erroring / not halting is itself a result. Two programs are equal if ``run p d == run q d`` for all ``d``.

An interpreter ``int`` has ``run int (p,d) = run p d``.
A compiler ``comp`` has ``run (run comp p) d = run p d``.
A compiler generator ``cog`` has ``run (run (run cog p) x) y = run p (x,y)``.
A specializer ``s`` has ``run (run s (p,x)) y = run p (x,y)``.

The Futamura projections:

let out = run p (x,y)
let res = run s (p,x) -- residual program
run res y == out -- by specializer property
let gen = run s (s,p) -- generating extension
run gen x == res -- by specializer property
let cog = run s (s,s) -- compiler generator
run cog p == gen -- by specializer property

``run s (s,int)`` produces a compiler. Similarly passing the interpreter to a compiler generator ``run cog int`` produces a compiler.

Given a specializer, we can produce a compiler generator ``cog = run s (s,s)``. This compiler generator is self-generating, in that ``run cog s == cog``. The reverse is true as well: if we have a compiler generator ``cog`` that self-generates with some program ``s`` (``run cog s == cog``), then ``run s (s,s) == cog`` and ``s`` is a specializer.

For any 3 specializers ``i``, ``j``, ``k``, not necessarily distinct, ``run i (j,k)`` is a compiler generator. We can also produce this compiler generator from any compiler generator ``cog`` by doing ``run (run (run cog i) j) k``.




package flags for the RTS
those flags should be stored into ABI hashes in installed packages to avoid mismatching incompatible code objects.
feature flags, can depend on the OS / target (e.g. NodeJS or browser targets)

package system must handle host packages and target packages.
compiler plugins are in the host package set

(`GHC ticket <https://gitlab.haskell.org/ghc/ghc/-/issues/14335>`_).

Plugins still won't work for stage 1 compilers because of ABI mismatch: the
stage 0 compiler may produce code objects for the stage 1 compiler that are not
compatible with the code objects the stage 1 compiler produces.

Breaking change: currently GHC is able to compile its own plugins in confined
mode. In particular, it supports loading plugins from the "home package" (the
set of modules it is currently compiling). While GHC isn't multi-target, it
won't be able to build its own plugins. Cross-compilers such as GHCJS or
Asterius relies on two GHCs: one for the real target and one which targets the
compiler host. We probably should make GHC multi-target and
multi-package before we could get this change integrated upstream.

Make GHC multi-target
---------------------

GHC should be able to produce code objects for several targets:

- (not available in stage 1 because of ABI mismatch) its own host platform and
  compiler way (for plugins): ``-target self``
- its own host platform: ``-target host``. It targets the same platform as
  ``-target self`` by without the constraint of being linkable with GHC. Other
  options could be applied (``-debug``,  ``-profiling``, etc.).
- several other targets

We need a way to configure two external toolchain information (gcc, llvm, as,
ld, ar, strip, etc.): one for GHC plugins and another for the current target.
A bunch of work has been done making GHC read these things from the ``settings``
file rather than it be hard-coded at build time.

There are still some target dependant hard-coded information in GHC about
`Int64#/Word64#` primops (cf `#11953
<https://gitlab.haskell.org/ghc/ghc/issues/11953>`_, `#17375
<https://gitlab.haskell.org/ghc/ghc/issues/17375>`_, `#17377
<https://gitlab.haskell.org/ghc/ghc/issues/17377>`_), which @Ericson2314 and @hsyl20 are attempting
to fix in `!3658 <https://gitlab.haskell.org/ghc/ghc/merge_requests/3658>`_.

GHC needs to handle per-target package databases.

Making GHC multi-target does not make it able to produce code objects for
multiple targets in a single GHC session. In particular it can't build plugins
(``-target self``) and actual code objects for the real target in the same session
yet. We need to make GHC multi-package to support this.

Related: `#11470 <https://gitlab.haskell.org/ghc/ghc/issues/11470>`_

Make GHC multi-package
----------------------

Currently a GHC instance can only compile modules from a single package, then
called "home package". Making GHC multi-package would mean that we would have
several active "home" packages at the same time.

Suppose we have a package containing a plugin module P and another module M
using the plugin. In confined mode we can just build and load P before building
M. But in a cross-compilation settings, we would need to build P with `-target
self` and then load it before building M for the actual target. I.e. we would
have the same package built for different targets. Hence we would have two
active packages.

Multi-package also permits interactive (re)compilation of modules from several
packages (cf `#10827 <https://gitlab.haskell.org/ghc/ghc/issues/10827>`_).

@fendor is working on this as part of GSOC 2020.
@Ericson2314 is one of the mentors for this.

Related:

* https://github.com/ghc-proposals/ghc-proposals/pull/263
* https://gitlab.haskell.org/ghc/ghc/wikis/Multi-Session-GHC-API


Make iserv program reinstallable
--------------------------------

Allow on-the-fly build of the iserv program. Depending on the selected target,
GHC should build an iserv program executing on the host (but not necessarily
with the same way as the compiler) that can execute target code.

GHC distributions wouldn't have to provide several ``iserv`` programs for every
target. They could be downloaded from Hackage and built for the host (now that
GHC would be multi-target).

Related issue: https://gitlab.haskell.org/ghc/ghc/issues/12218

Make boot libraries and GHC reinstallable
-----------------------------------------

The long term goal it to make GHC behave like any other Haskell program, and the
boot libraries like any other Haskell libraries.

GHC should be able to rebuild its boot libraries with different flags. Similarly
to iserv programs, GHC distributions shouldn't have to provide boot libraries
for every target (in addition to the boot libraries used by the compiler).

Similarly we also want GHC itself and the RTS to be reinstallable using standard
Haskell tools. It means that GHC shouldn't need Hadrian to be built but should
behave like standard Cabal packages.

There are several subtasks to perform before we can achieve this goal:

#. Rather than having global build, host, and target platforms (and ways, see
   the next section), Hadrian should give each stage its own host
   platform. As GHC would be multi-target, we can infer the effective target of
   the ``stage n`` compiler by looking at the required host for the ``stage
   (n+1)`` compiler.

   Instead of having a list of stages, we could have a tree:

   .. code::

         0       -- (old stage 0) the bootstrap compiler version X
         |- 0    -- (old stage 1) compiler version X+1, same host as bootstrap,
         |  |       don't have `-target self` support because of ABI mismatch
         |  |- 0 -- (old stage 2) compiler version X+1 that supports `-target self`
         |  |- 1 -- same but with other build options (e.g. profiling enabled)
         |  |- 2 -- same but with other build options (e.g. debugging enabled)
         |
         |- 1    -- compiler version X+1, without `-target self` support,
         |  |       with other build options
         ....


#. GHC's configure script should be split up per-package (cf `#17191
   <https://gitlab.haskell.org/ghc/ghc/issues/17191>`_).
   Currently, a single top-level ``configure.ac`` file is used for several
   packages and the compiler itself.

   Rather than use a dummy ``--target`` when building the compiler itself
   (because it is now multi-target), and then real ones when building the
   libraries, we should just remove `--target` from the overall one.

#. Configure scripts should be avoided altogether. If we want to build GHC on
   non Unix-like hosts (like Windows without using MSYS2), we shouldn't use
   configure scripts.

#. Don't generate source files with an external tool that GHC/Cabal isn't aware
   of. Currently Hadrian generates several files:

   * Parser/Lexer (via Happy/Alex): cf `#17750 <https://gitlab.haskell.org/ghc/ghc/issues/17750>`_
   * primops (via genprimopcode)

   Related: `!490 <https://gitlab.haskell.org/ghc/ghc/merge_requests/490>`_

#. Build GHC in Nix.

   Writing a build system is very hard especially because we don't want to
   mix up wrong files: e.g. wrong external files picked (``.h`` header files),
   artefacts produced from previous builds, etc.

   There have been several tickets involving these kind of issues with GHC's
   build system (e.g. `Hadrian picking the wrong gmp header
   <https://gitlab.haskell.org/ghc/ghc/issues/17756>`_).

   `Nix <https://nixos.org/nix/>` is a purely functional build system that
   provides sandboxed builds and correct-by-construction caching. It would be
   great to be able to build GHC using `haskell.nix
   <https://input-output-hk.github.io/haskell.nix/>`_ to benefit from it.

#. Ancillary tools outside of the confined mode

   There's lots of low hanging fruit. @angerman Fixed some silly make rules for ``hsc2hs`` and ``unlit`` in the past.
   Haddock is confused between its rigid GHC API version bound and its conventional laxed constraints on the GHC version used to build it.

   Related: `Haddock #1129 <https://github.com/haskell/haddock/pull/1129/files>`_ fixing stage 1 build.

Fix Template Haskell stage hygiene
----------------------------------

Currently Template Haskell mixes up stages because it assumes that the confined
mode is used.

We should be able to specify/detect if an ``import`` is for a top-level TH splice
or not.

We should remove ``Lift`` instances for target dependent types (e.g. ``Word``,
``Int``, linux only types, etc.).

- see `this proposal <https://github.com/ghc-proposals/ghc-proposals/pull/243>`_


Don't use the external interpreter for Template Haskell
-------------------------------------------------------

Template Haskell code shouldn't be executed by the external interpreter because
its code should be executed on the compiler host, not on the compiler target.

It is especially true if the external interpreter use a simulator (e.g. Android,
iOS, etc.) to run the code: TH code can perform unrestricted IO (readFile) and
may expect to find some "source" data files. It is already an issue in confined
mode (e.g. what is the current working directory of an executed TH splice? `it
depends
<https://github.com/haskus/packages/blob/fe2d5ce59e190ec54ae0f42a30c3eeed46997d45/haskus-utils-compat/src/lib/Haskus/Utils/Embed/ByteString.hs#L53>`_)
but it is only worse with the external interpreter.

A sane way would be to assume execution of TH codes on the compiler host. We
should specify the interaction of TH splices with the filesystem. We should
perhaps add a Cabal field similar to `data-files
<https://www.haskell.org/cabal/users-guide/developing-packages.html#pkg-field-data-files>`_
(or reuse `extra-source-files`) to indicate which files are accessible via TH
code using a new method of the ``Quasi`` monad (e.g. ``qLookupDataFile ::
FilePath -> Maybe ByteString``). Actually this could be done right now to avoid
CWD related issues.

TH code should have dynamic access (i.e. not via CPP) to the target platform
properties (word size, endianness, etc.).

We should provide a way for TH code to query some stuff about the target code
via the target code (external) interpreter: e.g. ``sizeOf (undefined ::
MyTargetSpecificData)``. It could also be used to resolve quoted identifiers
that only exists in target code (e.g. evaluate ``'MyTargetSpecificData ::
Name``).

Executing code on the compiler host in every cases should enhance speed as TH
code is often used to perform syntactic transformations (e.g. ``makeLenses``)
which don't require target code evaluation.

Now how would we execute TH code:

#. Use the internal interpreter just like plugins.

   It requires a compiler with ``-target self`` support. Hence TH wouldn't be
   supported in the stage 1 compiler and still couldn't be used in GHC source
   itself.

#. Use another interpreter for host code.

   We could compile TH code with ``-target self`` but not all the way to producing
   code objects because we may not be able to load them (e.g. in a stage 1
   compiler). Instead we stop at a previous stage and interpret the intermediate
   representation:

   - Core interpreter: compile to down to Core and evaluate it (cf `proposal
     <https://github.com/ghc-proposals/ghc-proposals/issues/162>`_)

   - STG interpreter: same but for STG (e.g. `ministg
     <http://hackage.haskell.org/package/ministg>`_)

   - ByteCode interpreter: same but for ByteCode. It is similar to the current
     internal interpreter but we would need to refactor it to virtualize the
     interactions between the compiler and the interpreter (currently the
     internal interpreter treats the rest of the compiler as yet another native
     code library, just one that happens to be statically linked with the
     interpreter itself).


Cabal
-----

Cabal should understand cross compilation and bootstrapping.

#. ``Setup.hs``

   Cabal packages are built by a ``Setup.hs`` program running on the compiler
   host. Most of them use the same "Simple" one but some others use custom
   ``Setup.hs``, with dependencies specified in ``.cabal`` files.

   Once GHC becomes multi-target, Stack and cabal-install could use ``-target
   self`` (for stage >= 2 compilers) or ``-target host`` (for any stage
   compiler, including stage 1) to produce the actual program for the compiler
   host. It would ensure that ``Setup`` programs can always be built and run on
   the host.

   * ``-target self``: when it is available (stage >= 2) it allows the use of
     the same boot libraries as the compiler itself

   * ``-target host``: should always be available. However with stage 1
     compilers we can't reuse self packages (boot libraries of the compilers and
     the compiler package itself) because of ABI mismatch. There are two
     solutions:

     * a second set of boot libraries needs to be built for the host just as if
       we were building a stage 2 compiler (hence it may require reinstallable
       boot libraries)

     * Cabal should be aware of the bootstrapping relationships between
       toolchains (next item).

#. Cabal should be aware of the available toolchains

   Currently cross-compilers such as GHCJS and Asterius use two GHC compilers:
   one for the target and another for the host (used to build the former GHC,
   the compiler plugins and ``Setup.hs`` programs). It would be good to make
   Cabal aware of the different toolchains (including GHC compilers) at its
   disposal and their bootstrapping relation.

   * While GHC and Clang are multi-target, other tools like GCC are not so Cabal
     would already need a notion of per-stage tools. It's not that much harder
     to also make that available for GHC itself.

   * When using a stage 1 compiler that doesn't provide ``-target self``, one
     has the option to instead use the previous stage's compiler to build
     plugins, which will make the ABI match.

   Related: `#11378 <https://gitlab.haskell.org/ghc/ghc/issues/11378>`_

#. ``Setup.hs`` should be a regular Cabal executable component built like any
   other.  Cabal now is well established in its notion of distinct components
   per-package that interact just through their dependencies. What makes
   ``Setup.hs`` component different is:

   * the fact that other components of the package have a "this is my
     Setup.hs"-type dependency on it

   * the fact that it is built to be executed on the compiler host, not on the
     actual target.

#. Cabal needs to know the target and the dependencies of each component it
   builds, including ``Setup.hs`` components as per the previous item.

   cabal-install's solver already does have some understanding of disjoint
   dependency graphs (via `qualified goals
   <https://www.well-typed.com/blog/2015/03/qualified-goals/>`_). E.g. when
   trying to build package ``foo`` which depends on ``base``, it tries to find a
   ``base`` package for ``base`` and another for ``foo.setup.base`` (they may
   not be the same).  We would have to extend this mechanism to consider target
   and stage information (as discussed in the context of Hadrian above).

   This would be a *huge* step towards the goal of GHC not needing bespoke logic
   in its build system.


Cabal: ``configure`` build-type
-------------------------------

Some Cabal packages use ``build-type: configure`` (see the `user manual
<https://www.haskell.org/cabal/users-guide/developing-packages.html#system-dependent-parameters>`_).
During the configuration phase, the package description is amended by a
``configure`` script producing a ``buildinfo`` file.

This only works on Unix-like systems and without additional parameters it
assumes that the target is the compiler host.

Portable packages (in particular boot libraries) shouldn't use this. They might
call ``configure`` in custom ``Setup.hs`` on Unix-like platforms though, passing it
flags to specify the actual target if necessary.

But for sake of unix-only packages it wouldn't be hard to teach Cabal to use
`--build`, `--host`, and other Autotools conventions. Autotools, after all, may
be nasty and crude, but does actually have not-so-bad support for cross
compilation thanks to GNU trying to sneak onto all manner of proprietary Unices
in the 1990s.

Remove platform specific CPP
----------------------------

GHC should expose a virtual package (like ``ghc-prim``) with target information
(e.g. word size, endianness) as values/types instead of using CPP to include
``MachDeps.h``.

Expressions using these values would be simplified in Core.

We could use Template Haskell instead of CPP in some cases. E.g.

.. code:: haskell

   foo :: Int -> Int

   #ifdef GHC_VERSION <= 806
   foo x = x + y
   #else
   foo x = x + z
   #endif

   -- becomes

   $(if ghc_version <= 806
      then [d| foo :: Int -> Int
               foo x = x + y
           |]
      else [d| foo :: Int -> Int
               foo x = x + z
           |]
   )

   -- or
   foo :: Int -> Int
   foo x = $(if ghc_version <= 806
               then [e| x+y |]
               else [e| x+z |]
            )


The advantage of the latter is that both quotes must parse as valid Haskell
code. However renaming and type-checking are performed lazily, which is what we
want because some names may not be available (e.g. ``y`` or ``z``) depending on
the condition (e.g. here ``ghc_version <= 806``).

Related:

* https://www.youtube.com/watch?v=YupkE1vsZ4o

when running byte code we must dynamically link the packages, and only in stage 2 and later do we have packages that GHC was built against
As the compiler quite often depends on features of libraries it depends on that are not guaranteed to be new enough in the bootstrap compilers package database, the first step is to augment the bootstrap compilers package database with those required packages to build the Stage 1 compiler. To do this, we compile this set of bootstrap packages with the bootstrap compiler.
stage 1 compiler
stage 1 package database
build stage 2 compiler with the stage 1 compiler using the stage 1 package database
ship with the stage 2 compiler). As such, the compiler is built with the identical libraries that it ships with. When running / interpreting byte code, we need to dynamically link packages and this way we can guarantee that the packages we link are identical to the ones the compiler was built with. This it is also the reason why we don’t have GHCi or Template Haskell support in the stage 1 compiler.

If we disable GHC features that require code execution (plugins and the internal
interpreter, hence Template Haskell too), we get a GHC that can produce code for
a different target. The price to pay is that this compiler doesn't support the
full language.

GHC itself is written with this language subset (it doesn't use compiler plugins
nor Template Haskell).

- use the multi-threaded runtime system or not
- support profiling or not
- use additional debug assertions or not
- use different heap object representation (e.g. ``tables_next_to_code``)
- support dynamic linking or not

These options are called "compiler ways". Some of them can be combined (e.g.
threaded + debugging).

Depending on the selected way, the compiler produces and links appropriate
objects together. These objects are identified by a suffix: e.g. ``*.p_o`` for an
object built with profiling enabled; ``*.thr_debug_p.a`` for an archive built with
multi-threading, debugging, and profiling enabled. See the gory details on the
`wiki <https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/compiler-ways>`__.

Installed packages usually don't provide objects for all the possible ways as it
would make compilation times and disk space explode for features rarely used.

If the selected way is not the same as the one used to build GHC and its boot
libraries, it breaks the confined mode assumptions: the produced objects can't
be dynamically linked with GHC.

GHC can build objects both for itself (i.e. using the way it has been built
with) and for the selected target way. By doing this, it can simulate the
confined mode by loading objects on the host that are different from the objects
produced for the target, with the hope that the two ways have no observable
difference. Quoting the `wiki
<https://gitlab.haskell.org/ghc/ghc/wikis/remote-GHCi>`__: "The way this is done
currently is inherently unsafe, because we use the profiled .hi files with the
unprofiled object files, and hope that the two are in sync."

Another solution is to use an external interpreter.


Breach #3: external interpreter
-------------------------------

The idea behind the external interpreter is to delegate the execution of the
target code to another process (called ``iserv``). This process can then delegate
to another one hosted on another platform or in a VM (e.g. NodeJS) if necessary.

GHC performs two-way communication with ``iserv`` process to send ByteCode to
evaluate, to ask for package to be linked, etc. During code execution, the
``iserv`` process may query the host GHC (e.g. when Template Haskell code is run,
it may query information about some ``Names`` and these information live in the
host GHC).

GHC spawns a different ``iserv`` process depending on the selected target way:
``ghc-iserv-prof``, ``ghc-iserv-dyn``, etc. This allows the ``iserv`` process to load
target code objects which have not been built with the same way as GHC.

A different external interpreter can be specified with the ``-pgmi`` command-line
option.

1. Using the external interpreter in GHCi makes sense because it allows the
   execution of the code produced for the target on the compiler host (or
   remotely but it is internal to the ``iserv`` process and GHC isn't aware of
   it).

2. Using the external interpreter to execute Template Haskell code doesn't
   really make sense: TH code is similar to plugin code in that it has access to
   some compiler internals (``Names``, etc.), it can modify the syntax tree and
   it can perform IO (read files, etc.). Morally it should be built so that it
   can be linked with the compiler and executed on the host.

3. Compiler plugins don't work at all with the external interpreter (see `#14335
   <https://gitlab.haskell.org/ghc/ghc/issues/14335>`_). It is because they
   directly depend on the ``ghc`` package and assume they are going to be linked
   with it. Executing compiler plugins in the external interpreter would mean
   that the communication protocol between ``iserv`` and GHC would need to be
   extended to support everything a compiler plugin can do. As compiler plugins
   can do virtually anything in the compiler, it would mean that most GHC
   datatypes would need to be serializable, most functions explicitly exposed,
   etc. Moreover we would have to deal with the discrepancy between host and
   target datatypes (word size, etc.). It probably won't happen.

External interpreter links:

* https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/external-interpreter
* https://gitlab.haskell.org/ghc/ghc/wikis/remote-GHCi


