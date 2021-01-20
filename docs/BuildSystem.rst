Build system
############

Although build systems are often an afterthought in programming language design, they interface with the compiler in several areas, so it is better to integrate the build system into the compiler as a library. That way intermediate results such as checked/optimized functions can be stored efficiently and rebuilt only when needed. What is usually called "separate compilation" is actually incremental compilation. Compilation's goal is to produce a single executable / DLL, so these "separate" files are still part of the same assembly. Object files are simply one way of storing intermediate results so that compilation work done on the files can be shared; but it does not handle some patterns, e.g. recursive references. Also, the compiler's include-following mechanism can be tightly integrated with the build system, so that generated files can be created before they are used.

.. graphviz::

  digraph foo {
    rankdir=LR;
    {
    rank=same
    IKey [label="Initial keystate"]
    IKey2 [label="Modified keystate"]
    }
    subgraph cluster_keys {
      style=filled; color=lightgrey
      node [style=filled,color=white]
      Files, Network, Database, Journal, Time [shape="circle"]
    }
    {Files; Network; Database; Journal; Time} -> IKey

    IKey -> "Rule engine"
    "Rule engine" -> OKey
    {
    rank=same
    OKey [label="Output keystate"]
    OKey2 [label="Updated keystate"]
    }

    "Traces" -> OKey [dir=back]
    "Rule engine" -> "Traces"

    {IKey2; "Traces"} -> "Incremental rule engine"
    "Incremental rule engine" -> OKey2
  }

Broadly, a build system is an execution engine combined with logic for incremental computing.
A clean build starts with (user-editable) inputs, which the execution engine executes to completion to produce the set of outputs. For most builds, we also have reference to the previous runs of the engine. In the simplest case it is only the trace of the previous run that is available, but a caching build system with constructive traces can maintain many sets of traces in parallel and interpolate between them.

Logically, there is a clear separation between inputs and outputs, but if the key is external and the storage for input and output is the same we may end up modifying the initial state. This is presumably desired behavior, e.g. a source code formatter tidying up. It can be avoided by redirecting the input or output to a non-overlapping location. If an input is overwritten before it is read, then it is too late to use, so actually the key is only an output key.

If an output is modified or deleted, the clean build semantics dictates that it will be regenerated from the inputs. We could think of a separate option allowing a "what-if" query, "what would the output be if the task that made the output generated this". But the semantics is murky - to preserve the modification, we must disable all of the tasks that would modify the generated file, which if they write other files means that the build will contain out-of-date files.

Algorithm
=========

Files are too big to store during execution. so during re-execution, the file may have changed / been deleted (damaged).

The simplest solution is to re-execute the thunk if any of the written files are damaged. But we can also ignore the damage and avoid re-execution, in various cases.

To formalize this:
we have
- times which form a poset / DAG. times compare equal, less than,greater than or incomparable/concurrent
- key names, key values. these compare for equality. here we use strings abc for names and integers 123 for values
- thunks. each thunk runs at its own time, reads stuff, then writes stuff - we store the name/value read/written

so an example database might be
time RW key value
1 R a 1
1 W b 1
2 R b 1
2 W c 1

Then during an incremental run we start with a list of damaged and dirty keys. Damaged keys are writes that are no longer valid (disk state is different), while dirty keys are reads that are no longer valid (computed state is different).  Dirtyness is caused by a change in the build data or nondeterminism during a re-execution. Damage is caused by external reasons or by a thunk overwriting the output of another thunk.

dirtyR 2 b
damagedW 2 c

A thunk is dirty if it has dirty reads and damaged if it has damaged writes. A thunk is clean if all of its recorded reads and writes (inputs and outputs) are in the state they were in at the end of the thunk's execution. So a thunk is always clean immediately after it is executed. A clean thunk does not need to be re-executed during a run, unless an earlier re-executed thunk changes one of its outputs and makes the clean thunk dirty. A dirty thunk must be re-executed, unless the flow of execution changes and the thunk is not called, or the inputs are magically restored to their recorded state.

A damaged thunk may or may not need to be re-executed depending on whether the damaged writes are used. If the results aren't used, the damage can be ignored, but otherwise the thunk must be re-executed to regenerate the necessary data. To determine this, Stroscot runs a global simulation of the build the first time a damaged thunk is encountered, with the following model:

* Clean thunks produce their outputs as normal
* Dirty thunks mark all outputs as dirty and continue execution as if they were clean
* Any thunks which reads a dirty key is considered dirty
* Running or executed thunks are considered dirty, similarly thunks loaded cleanly are considered clean.
* Damaged thunks mark their damaged outputs as damaged, but other outputs as clean
* When a dirty thunk reads a damaged output, the damaged thunk is marked as dirty and thunks are re-executed from there

This model is a simplification, in particular it doesn't cover:

* A re-executed thunk that adds a dependency on damaged data
* A thunk that doesn't execute because its data is unchanged

In practice this simulation can be expensive because it might require multiple traversal of the build graph. So it can be disabled globally or on a per-thunk basis, or the computation can be overriden to handle damage in a different way.

Another possibility is to do the "restart execution from the damaged thunk on detection of a read" strategy with the actual build, instead of in a simulation. But this is tricky because we want to avoid the state becoming a mixture of two runs (the old state read before re-execution and the new state after re-execution triggered by some unrelated dirty key), so we would have to have some way of checkpointing the state when encountering a damaged thunk. Also, when we execute a unit, all the work done after that unit is thrown away, so we could encounter a lot of re-executions, in particular the case where we execute a unit B, then go back and re-execute a unit A due to damage, then have to execute B another time due to A changing C changing input to B.

Notes
=====

For the task graph, we have a nontrivial requirement for soundness, similar to :cite:`erdwegSoundOptimalIncremental2015`:

* If a task depends on a provided key, the task providing the key must have been run first.

An easy way to ensure this is to construct a function mapping from provided files (keys) to tasks, and then have a library function for requiring keys which uses the map to require the task and then the key. Unfortunately in a dynamic build, a direct map like this is not always available, and so the requirement is relaxed to allow indirect dependencies. For example, we may have include headers that are picked up in a search path directory listing. To deal with this directly, we would need to introduce build logic into the search mechanism and download dependencies when seeing ``#include``. But a phase separation handles it fine with minimal changes - we download a large list of packages first, trace file accesses to see which are used, and add post-facto dependencies.

Without an initial list of changed keys, we will have to check all the keys individually. A bigger question is whether up-propagation of dirtyiness can be avoided. The intuition is that most dependency graphs are tree-like and so going up is roughly :math:`\log(n)`, which seems acceptable. There are some dependencies (e.g. small common functions) which have a huge reverse dependency list, but changing those requires a full rebuild anyway so the overhead is dwarfed, and the changes might not propagate up the tree.

Handling provided files is tricky; consider adding an automated source-code formatter task. All of the files are provided by the formatter task. We want the formatter to run if you hand-edit the files, so the files are also dependencies of the formatter task. So it is a cyclic graph.

If you modify provided files, they become source files and are not considered out-of-date. If you delete them, they will be rebuilt.

Unlike Shake, tasks are not keys; it is a two-level graph like Pluto. Task identifiers have a byte serialization like keys, but they are in a different namespace. The store maintains dependency lists of key and task identifiers for each task, but tasks do not store versions the way keys do.

I looked at implementing cycle detection like Pluto, but it seems you have to kill all the in-progress builders involved in the cycle to start the cycle manager, which really screams "hack!". The cycle manager can simply be a unit to begin with.

Steps: Often tasks do things in sequence, like creating a file and then moving it to an installation directory. Properly, these are separate tasks, so that we can do dependency tracking and only move the file if it has changed. We name each step like "BuildStep x 0".

Reconstruction: to build mutable systems, that write and overwrite files, we have to expand our notion of consistency, so that overwriting to a file only makes the latest edition of a file out-of-date, rather than forcing the build to start at the beginning. When re-rexecuting a task, we must first reconstruct its environment, as it may have been changed by something later in the build.

Reconstruction is more like rewinding

Technically, our notion of consistency is based on a "clean build", with the filesystem initialized to source files and all generated files deleted. For true replication, when re-building a task T, we would have to delete all the build files generated by tasks depending on T, in case T accidentally read "from the future". But this is more easily detected after-the-fact, particularly in the case of parallel builds.

* neural net that does 5 runs/subtasks which write back to file


The pipeline of a build system is as follows:

* We start with a changelist of "pending" keys, i.e. keys that have changed since our last build. The prototypical example is a list of changed files from a file-watching daemon, but it can also include volatile information such as external tool version numbers or FTP server listings or finer keys such as individual AST nodes. We could also use the list of all keys from the previous build, skipping the watcher altogether.
* We go through the list and scan the data for each key. If a key has actually changed, we mark all the tasks that have used it as dirty.
* We also propagate dirtiness up the pre-built task/key graph; every task that depends on a dirty task is marked as needing a recheck.
* After scanning all the keys, we go down starting from the top-level task. (We could start the build earlier by doing speculative execution, but scanning is cheap) We want a suspending build system :cite:`mokhovBuildSystemsCarte2020`. So there must be some way to suspend the current task when it calls a sub-task, probably just continuations like how Shake does it.
* When a task is called, we first check its state to determine whether it needs to be re-run. Dirty tasks are run immediately. Loaded tasks can be skipped immediately, as can tasks stored in the database that have not yet been marked. Otherwise, for rechecks, we run through the serialized dependency list and re-check the keys / subtasks in order (and in parallel if the subtasks are parallel). When the task is finished its state is marked as loaded / error.
* Before running a task, we clean up old build results, if any, i.e. delete all provided keys (outputs) that are still present. After running a task we store its (keyed) outputs with either verifying or constructive traces.
* To prune the store (which is a bad idea if there are multiple configurations that build different subsets), we can do as above and also load all the subtasks of present tasks. Then anything not loaded is not needed and its files etc. can be deleted.


Exceptions
----------

Shake tries to be exception-safe, handling GHC's broken `asynchronous exception system <https://www.fpcomplete.com/blog/2018/04/async-exception-handling-haskell/>`__. The system is broken because it is so complicated that nobody can agree on the desired behavior / correct form of even simple examples. The prototypical example of using it is `bracket <https://hackage.haskell.org/package/unliftio-0.2.13.1/docs/UnliftIO-Exception.html#v:bracket>`__:

  bracket :: MonadUnliftIO m => m a -> (a -> m b) -> (a -> m c) -> m c
  bracket before after thing = withRunInIO $ \run -> EUnsafe.mask $ \restore -> do
    x <- run before
    res1 <- EUnsafe.try $ restore $ run $ thing x
    case res1 of
      Left (e1 :: SomeException) -> do
        _ :: Either SomeException b <- EUnsafe.try $ EUnsafe.uninterruptibleMask_ $ run $ after x
        EUnsafe.throwIO e1
      Right y -> do
        _ <- EUnsafe.uninterruptibleMask_ $ run $ after x
        return y

Here we use 4 operations: mask, try, uninterruptibleMask_, throwIO. mask shields the cleanup action from being attacked by asynchronous exceptions, allowing exceptions inside restore. try catches exceptions and allows cleanup to occur. uninterruptibleMask_ blocks interrupts from interrupting the after handler. Finally throwIO rethrows the exception, so that any exception inside the after handler will be swallowed.

Trace journal
=============

A robust build system design fundamentally depends on keeping a database of build traces. In particular to rebuild a command like ``cat src/*`` we must store the file list so as to detect deleted/added files. We could store this in a file, but an append-only journal is crash-tolerant and less HD-intensive. Since file paths have lots of redundant components, some lightweight streaming compression like lz4 is appropriate.

We record all of the process/thread semantics, with fork, locks, wait/signal, etc. as well as its I/O. The tasks's version number / digest of its source code is also relevant. Reading the journal back, we end up with a list of interleaved thread traces.

Requesting execution of other tasks can be done sequentially or in parallel.


There are 3 main operations that show up in a task's trace:

* writing a key
* reading a key
* requesting execution of other tasks

To correctly build software, we assume that the task is deterministic besides the operations recorded in its trace - so the task can be skipped if all of its inputs and generated files are the same.

In-memory
---------

In-memory keys are the simplest to handle, because they're small and we can simply store the whole value, and also because we don't have to worry about external modification. We record a write in our journal as "write key xyz = ..." and a read as "read key xyz = ...". Then the trace is invalid if we read something different from what was written, or if the key was never written.

If the key contents are large, we can intern it - writes as "write key xyz is interned to #5 = ...", and reads as "read key xyz from intern #5". We can't use the key itself because there might be multiple writes to the key.

Files
-----

Files are a little trickier because storing the whole contents of the file in the journal is infeasible. Instead we journal a proxy of the contents, stored in-memory. So writes look like "write file f with proxy p" and reads are "read file f with proxy p". If our dependency tracking is perfect then the reads can use the in-memory value of p calculated from the writes. Also, since files are exposed to user control, on startup the build system must scan all the files. This can take several minutes with a digest-only algorithm or a few seconds with modtimes.

trivial proxy
  Sometimes we want to ignore the file contents and always/never do an action. In such a case we can use a trivial proxy. There are two types, "always rebuild" and "never rebuild". In the never case, the rebuild can still be triggered by a different file.

version number
  For toolchains in small projects, the version number from running ``gcc -V`` etc. is often sufficient. But modtime is more robust.

file size/permissions/inode number
  Checking the file size is fast and cheap as it's stored in every filesystem. This catches most changed files, but is incomplete since a modification may keep the same file size. File permissions can also be relevant, if they are changed from the default.

modtime/device/inode number
  As opposed to make's simple "is-newer" comparison, storing the full mtime value is pretty accurate. mtime changes at least as often as the content hash changes. There is a small risk that a file archiver or inaccurate clock will set the timestamp to collide with the old one and the change won't be detected. The device/inode number detects replaced files, e.g. if you ``mv`` a file onto another one. The real disadvantage is over-rebuilding, due to ``touch`` and similar. ctime and atime update even more frequently than mtime, so they don't help. btime / creation time might be useful, in a manner similar to inode number. Simply checking all the mtimes sequentially is very efficient due to filesystem caching and it can be made even more efficient with various tricks.

digest
  A digest computed from the contents. There is a remote risk that the file will change without its digest changing due to a collision, but otherwise this detects changes accurately. The disadvantage of digests is that they are somewhat slow to compute, requiring a full scan of the file. But various virtual filesystems store precalculated file checksums, in which case those would be better to use than mtime. There are fast hash algorithms like `xxHash <https://cyan4973.github.io/xxHash/>`__ that have throughput faster than RAM, so the main bottleneck is the I/O. Looking at the `benchmark <https://github.com/Cyan4973/xxHash/wiki/Performance-comparison>`__, and fruitlessly googling around to find other hashes not listed there (fnv1, murmurhash, siphash), it seems xxHash3 / xxHash128 are the fastest. But, if we are going to share the files over a network then one of the SHA's or BLAKE3 might be better to prevent file-replacement attacks. There is also the Linux Kernel Crypto API using AF_ALG but it seems to be slower than doing it in user-space.

watcher/change journal
  We can run a filesystem watching service like Watchman, on Windows use the `USN journal <https://en.wikipedia.org/wiki/USN_Journal>`__, strace all running programs, or redirect filesystem operations through a FUSE vfs. In each case, we get a list (journal) of all changes since some arbitrary starting point. If the journal covers all of the time since the last build, we have a full list of changes and don't need anything else; otherwise we need to supplement it with one of the other methods.

We can construct modes from the various combinations:

* digest-only, if modification times on your file system are missing or don't update on changes.
* modtime-only, if your timestamps change mostly in sync with the file content
* modtime-then-digest, if you could use modtimes but want to avoid spurious rebuilds. In particular git touches a lot of files when switching branches, vim copies over the file so its inode changes frequently, and scripts/you can write identical files.
* modtime-then-some-digest, skipping digests for generated files as they're large and change with almost every rebuild. Generated file modtimes can be kept constant by writing to a temporary file and only replacing the output if it's different.
* watcher-only, if your watcher runs continuously or if you delete all files after every run
* modtime-then-watcher, if your watcher's change journal is incomplete
* modtime-then-watcher-then-digest, to get the fastest file tracking and fewest rebuilds

io_uring
~~~~~~~~

It's a little overkill, but the io_uring interface on Linux allows batching up calls asynchronously, which can speed up stat() by 20%. For read() parallelism is likely counterproductive, as xxHash is I/O bound and parallelism turns sequential reads into random reads.

Tracing
~~~~~~~

Specifying a lot of file dependencies manually is tedious and error-prone, although if a script is written from scratch it is not too difficult to add read/write tracking. So instead we want to use automatic tracing. There are various tracing methods:
* library preloading with fsatrace: fails on static linking, Go programs, and Mac system binaries
* ptrace with BigBro-fsatrace: Linux-only at present, might work on Windows/Mac eventually.
* chroot with FUSE: mount real system at ``/real-system/``, FUSE system with all files ``/x`` as symlinks to ``/real-system/x``. The program shouldn't access ``/real-system/`` directly. Handles all programs, even forking/multiprocess programs like make, and gives build system the abilities to hide new files and generate files on-demand. Requires Linux + root.

When we get back file paths from these tracers, they are usually absolute paths, or paths relative to the working directory. But we want standardized paths - if the build doesn't need to be copied/moved, then e.g. the home directory path should be omitted. Rattle's solution of named relative directories seems reasonable. Basically, if we have ``NAME=/x/y`` and a path ``/x/y/z`` then we shorten it to ``$NAME/z``, similarly expanding the name, and we sort the list of names to do this efficiently (or maybe use a tree?).

Network
-------

Often we wish to fetch data from over the network. There are a few common protocols:

* HTTP downloads: we can use wget, curl, aria2, or a custom library. The caching headers (Last-Modified & ETag) are important for re-using old downloads.
* FTP: this can be treated similarly to HTTP
* Git, Bittorrent, IPFS: these are content-addressed stores so keeping track of the hash is sufficient

A more complex example is deploying a container to AWS. The inputs are: all the configuration details for the host, the container image itself, and secret credential information. The output is a running instance or else a long log file / error message. But the running instance cannot be checksummed, so we must use some proxy criterion - the easiest is to redeploy if any inputs have changed, but we could also use a script to interrogate the running instance over the network.

If there are multiple containers that depend on each other, we have to encode the restarting behavior somehow. The easiest is probably to write a single script that takes all the configuration and starts up the containers in order, but this duplicates the build system task scheduling logic.

Pipeline
========

A task's state evolves as follows:

.. graphviz::

    digraph foo {
        rankdir=LR;
        Recheck -> Dirty
        Dirty -> Running
        Running -> Loaded
        Running -> Error
        Recheck -> Loaded
    }

More notes
----------

if your build system is broken then you can't fix it with the ``touch`` utility. so a command ``--touch`` that forces files to be invalid seems necessary, although it wouldn't be needed normally.

What happens when a file is written to more than once.

::

  et:
    run d
    ans = o + e

  dt:
    run c
    o = y + d

  ct:
    run b
    y = o + c

  bt:
    run a
    o = o + b

  at:
    o = i + a

Here ``at``, ``bt``, and ``dt`` write ``o``. Let's say the first run is ``i=a=b=c=d=e=1``, so our trace journal is:

::

  etm: execafter dtm et
  dtm: execafter ctm dt
  ctm: execafter btm ct
  btm: execafter at bt
  at: read i = 1
  at: read a = 1
  at: write o = 2 { v = 1 }
  bt: read o = 2 { v = 1 }
  bt: read b = 1
  bt: write o = 3 { v = 2 }
  ct: read o = 3 { v = 2 }
  ct: read c = 1
  ct: write y = 4
  dt: read y = 4
  dt: read d = 1
  dt: write o = 5 { v = 3 }
  et: read o = 5 { v = 3 }
  et: read e = 1
  et: write ans = 6

To keep the graph a DAG, we have split ``o`` into ``o1,o2,o3`` (the ``v`` version numbering):

..
  ([a-z]): exec ([a-z])
  $1 -> $2 [style=dotted, color=grey]

  ([a-z]): read ([a-z]) = ([0-9])
  $1 -> $2 [label="$3"]

  ([a-z]): write ([a-z]+) = ([0-9])
  $2 -> $1 [label="$3",color=blue]

.. graphviz::

    digraph multi {
        rankdir=RL
        node [shape="rect",fontsize=20]
        at, bt, ct, dt, et, btm, ctm, dtm, etm [shape="circle"]
        o1 [label="o₁",margin="0,0"];
        o2 [label="o₂",margin="0,0"];
        o3 [label="o₃",margin="0,0"];

        etm -> dtm [style=dotted, color=grey,penwidth=3]
        dtm -> ctm [style=dotted, color=grey,penwidth=3]
        ctm -> btm [style=dotted, color=grey,penwidth=3]
        btm -> at [style=dotted, color=grey,penwidth=3]
        etm -> et [style=dotted, color=grey,penwidth=3]
        dtm -> dt [style=dotted, color=grey,penwidth=3]
        ctm -> ct [style=dotted, color=grey,penwidth=3]
        btm -> bt [style=dotted, color=grey,penwidth=3]

        at -> i [label="1"]
        at -> a [label="1"]
        o1 -> at [label="2",color=blue]
        bt -> o1 [label="2"]
        bt -> b [label="1"]
        o2 -> bt [label="3",color=blue]
        ct -> o2 [label="3"]
        ct -> c [label="1"]
        y -> ct [label="4",color=blue]
        dt -> y [label="4"]
        dt -> d [label="1"]
        o3 -> dt [label="5",color=blue]
        et -> o3 [label="5"]
        et -> e [label="1"]
        ans -> et [label="6",color=blue]
    }

* ``(i,a,b,c,d,e,o,y,ans)=(1,1,1,1,1,1,5,4,6) [default]``: nothing is run
* ``{ans = 0}`` or ``{e = 0}``: run ``et``
* ``{o = 0}`` or ``{d = 0}``: run ``dt``, and ``et`` if ``o != 5``
* ``{a=0}`` or ``{b=0}`` or ``{c=0}``: run ``at,bt,ct,dt``, and ``et`` if ``o != 5``
* ``{b=0,o=2}``: run ``bt,ct,dt``, and ``et`` if ``o != 5``
* ``{c=0,o=3}``: run ``ct,dt``, and ``et`` if ``o != 5``
* ``{y=0}``: run ``at,bt``, ``ct`` if ``o != 3``, ``dt`` if ``y != 4``, and ``et`` if ``o != 5``

::

  etm r
  etm r, dtm r [et]
  etm r, dtm r [et], ctm r [dt]
  etm r, dtm r [et], ctm r [dt], btm r [ct]
  etm r, dtm r [et], ctm r [dt], btm r [ct], at r [bt]
  at:
    if i changed || a changed
      run at
    else
      if o changed
        record o damaged
  bt:
    if o changed || b changed
      if o damaged
        run at
        if o still damaged
          error
      run bt
    else
      if o changed
        record o damaged




  changed = Set(i,b,o3)

  recheck = {}
  if {i,a,o1} & changed
    recheck |= a,b,c,d,e
  if {o1,b,o2} & changed
    recheck |= b,c,d,e
  if {o2,c,y} & changed
    recheck |= c,d,e
  if {y,d,o3} & changed
    recheck |= d,e
  if {o3,e,ans} & changed
    recheck |= e

  check e

  check(x):
    if !(x & recheck)
      return

    for( deps)


   || (o != 2 && (o != 4 || x != 0))
    run a
  if a ran || o = 2
    run b
  if o != 4 || ans != 3
    run c



  c: check b
  b: check a
  a: if i=1
  a:  write o1 2
  a: else
  a:  rerun a
  a:  read i <i>
  a:  write o <i+1>
  aL return

  a: i != 1, rerun
  a: read i 0
  a: write o 1
  a: return
  b: a ran, rerun
  b: read o 1
  b: write o 2
  b: return
  c: o != 4, rerun
  c: read o 2
  c: write ans 1
  c: return


* ``(i=1,x=0,o=4,ans=3)``: nothing is run
* ``(1,0,4,_)``: ``c`` disabled. nothing is run
* ``(1,0,_,3)``: ``a,b`` disabled. run ``c``
* ``(1,0,_,_)``: ``a,b,c`` disabled. nothing is run
* ``(_,_,4,3)``: nothing disabled. run ``a,b``. If ``o != 4`` run ``c``
* ``(_,_,4,_)``: ``c`` disabled. run ``a,b``
* ``(_,_,_,3)``: ``a,b`` disabled. run ``c``
* ``(_,_,_,_)``: ``a,b,c`` disabled. nothing is run





-  redo-stamp records the checksum of the target in its database after
    building a target. Any data may be passed in. Any downstream target remembers that
    checksum in its list of dependencies; if it changes
    later, then the downstream target needs to be rebuilt.
    There is no need to actually recalculate any checksums
    when checking dependencies in the future. No special
    filesystem support is needed.

-  declare dependency on the toolchain:
    the rule for each target can track which parts of
    the toolchain were used while building, then
    retroactively declare a dependency on those.

-  If you pass variables on the command line, like
    ``CFLAGS=-O2``. Write
    CFLAGS to a file, atomically replacing it only if the
    content differs, and depending on that file.

-  track build system changes, per rule, and
    cause rebuilds for these cases.
    auto-declare a dependency on the rule file used for a
    given target. When you edit a rule, the affected targets
    are automatically rebuilt.

shake staunch mode: if an error is encountered during the middle of a build, unless --keep-going is specified we want to stop the build. we can stop all the threads immediately by sending cancel commands, or we can wait until each command finishes to interrupt. it's a bit of a corner case

Package manager
===============

A language also needs a package manager. When a task is requested, and package management is enabled, the task is checked against a list of prebuilt tasks and if so all of the task's provided keys (files) are downloaded instead of the task being built, verifying their cryptographic hashes/signatures. We also need a way to create packages from a build tree.

The list of files can be kept accurate by a filesystem access tracer or restricting the build scripts. A tracer will also pick up source files, intermediate object files, etc., but most people who use a package manager do not rebuild their intermediate steps and want the smallest possible package sizes. So we need some way to mark these scratch files; the easiest requirement is that the task delete all the junk data, as packaging a nonexistent file/directory is simply verifying that it doesn't exist on the target system.

There are also some filesystem convention/naming issues, in particular different layouts on different systems and allowing per-user installs, but Conda has worked out reasonable solutions for these, relative pathhs and so on.

A useful feature not implemented in most package managers is P2P distribution, over Bittorrent or IPFS. Trust is an issue in theory, but in practice only a few nodes provide builds so a key ring is sufficient. Turning each tarball into a torrent file / IPFS CID and getting it to distribute is not too hard, the main issue seems to be scaling to thousands of packages as DHT performance is not too great (Bittorrent is `not too great <https://wiki.debian.org/DebTorrent#line-42>`__). There are some notes `from IPFS <https://github.com/ipfs-inactive/package-managers>`__ and various half-baked package managers like ``npm-on-ipfs``.

In a long-running system, the number of prebuilt packages could grow without bound. We need a mechanism to clean out the archives when space becomes limited.

Edges are bidirectional. To fix the GC problem, we use weak references for back edges, but strong references for memo table entries, so that from the GC’s point of view, all DCG nodes are always reachable. To implement safe space reclamation, we also implement reference counting of DCG nodes, where the counts reflect the number of strong edges reaching a node. When DCG edgesare deleted, the reference counts of target nodes are decremented. Nodes that reach zero are not immediately collected; thisallows thunks to be “resurrected” by the swapping pattern. Instead, we provide aflushoperation for memo tables that deletesthe strong mapping edge for all nodes with a count of zero, which means they are no longer reachable by the main program.Deletion is transitive: removing the node decrements the counts of nodes it points to, which may cause them to be deleted.An interesting question is how to decide when to invokeflush; this is the system’seviction policy. One obvious choice is toflush when the system starts to run short of memory (based on a signal from the GC), which matches the intended effect of theunsound weak reference-based approach. But placing the eviction policy under the program’s control opens other possibilities,e.g., the programmer could invokeflushwhen it is semantically clear that thunks cannot be restored. We leave to future work afurther exploration of sensible eviction policies

Linux distribution
==================

Once we have a package manager we can build a Linux distribution. Compared to a user-level package manager, a system-level package manager must be built a bit more robustly to handle crashes/rollbacks. It also needs various build system hooks for dealing with tricky/non-standardized installation procedures, e.g. putting kernel/initrd images into the boot manager, building in a container with overlayfs to guard against untrustworthy packages, and using auditd to identify file dependencies in a bulletproof manner. As a basis for the distribution we can use small distros like LFS and Buildroot. It would also be good to figure out some way to import data from bigger distributions like Arch, Gentoo, or NixOS. Cross-compilation is a goal, but it isn't strictly necessary and it's easily broken anyways.

The goal of the Linux distribution, compared to others, is automation: all package updates are automatic, and packaging new software is as simple as giving a package identifier / URL (and dependency information or build instructions, for C/C++ projects or custom build systems). Language-specific package repositories have grown to be bigger than most distros, so providing easy one-line installation of them is paramount.

Package pinning is an issue, to handle broken software and stale dependencies. A new release of a tool might just not work; then it needs to be pinned to the old version. In contrast, a library update might break only a few packages; the distro should then package multiple versions of the library and build most packages with the new libary while pinning the library to the old version for the specific breakages.

The key question is where to store the multiple versions of the libraries. For a basic path like ``/usr/lib/libfoo.so.1``, we can put a hash ``123456`` in various places:

1. ``/usr/lib/libfoo.so.123456.1`` or ``/usr/lib/libfoo.so.1.123456`` (filename version)
2. ``/usr/lib/123456/libfoo.so.1`` (Debian multiarch layout)
3. ``/usr/123456/lib/libfoo.so.1`` (NixOS layout)
4. ``/123456/usr/lib/libfoo.so.1`` (multisystem layout)

The multisystem layout isn't useful, as the point of ``/usr`` is to allow a separate partition.

For the filename version, simply renaming it doesn't work, as there is a symlink ``/usr/lib/libfoo.so.1`` and its target is ambiguous if there are multiple versions. But if we modify the soname we can include the hash anywhere in the soname. Although we could `detect ABI changes <https://lvc.github.io/abi-compliance-checker/>`__, versions aren't linear in general so it has to be a hash instead of a sequential number. The soname can be set with a linker wrapper. The library will still have to be renamed during installation to include the hash. The library can then be linked with by setting a symlink from ``libfoo.so`` to the real version, as usual. ldconfig should work unmodified. Prebuilt binaries can be patchelf'd, but using ``--replace-needed`` rather than ``--set-rpath``.
Where this solution breaks down, however, is with data files. Maintaining hashed file versions like ``/usr/share/foo/foo-123456.jpg`` would require patching every application to look things up in the right place. So the only option seems to be using a hashed layout, ``/usr/share/foo-123456/foo.jpg``. But autoconf only has the option ``--datarootdir`` to change ``/usr/share``; it doesn't have a standard option to rename the subdirectory. So once again we'd have to manually patch every package. The only feasible option is to introduce another layer, ``/usr/share/foo-123456/foo/foo.jpg``. But that's clearly the Debian layout.

Comparing Debian and NixOS, the NixOS layout has the advantage of putting every package in its own directory. Installing thus does not have to worry as much about stray files conflicting. With split outputs, this is not as much a benefit to the user, because the documentation will in a separate package and hence not findable by just browsing the package directory, but the advantage for scripting is still there.

To include the hash in the SONAME like in the filename version, we should link with absolute paths (or relative paths, they would work too). There is `some work <https://github.com/NixOS/nixpkgs/issues/24844>`__ in NixOS to do so, but the current rpath solution works too (albeit slower).

Nix hardcodes the paths of most binaries. For the rest, resolving binary names ``foo`` to paths ``/usr/bin/foo-12345`` is not trivial. A global view doesn't work because we could have two binaries who call different versions of a binary. Instead we could make a pseudo-filesystem like devfs or ``/proc`` but for the system path; this can provide the necessary pid-dependent view as a symlink tree ``/path/foo -> /usr/bin/foo-12345``; even FUSE should be sufficiently fast since it is just one ``open()`` call and it doesn't have to handle the actual I/O. Currently NixOS uses environment variables, global symlinks in `/run/current-system/`, and chroot containers.

Automation system
=================

Although a distribution is sufficient for setting up a single computer, to set up multiple computers it is more complicated. Salt provides a command-execution agent, but the commands are not idempotent. We want a map from packages to their latest versions or pinned versions. The 'autoremove' option is on by default because packages being secretly installed is a bad idea. But with autoremove off, packages are left installed on the system if they aren't explicitly specified for removal.

Release monitoring
==================

Automating package updates requires finding new releases and then testing it. For the first part, unfortunately there is no standardized API. There is `Anitya <https://fedoraproject.org/wiki/Upstream_release_monitoring>`__, which solves some of this, and also `cuppa <https://github.com/DataDrake/cuppa>`__. But both of them work by writing backends/providers for each major hosting site.

Although the most recently modified / created version is usually the latest release, and hence it is easy to identify, some projects maintain multiple versions, so that newer files might actually be security updates to old versions rather than the latest version.

We can write our own project scraper:

* KDE, Debian: There is a ``ls-lR.bz2`` / ``ls-lR.gz`` file in the top level with a directory listing with timestamps and filesizes.
* GNU, `Savannah <http://www.gnu.org/server/mirror.html>`__, GNOME, Kernel.org, X.org: We can get a directory listing from an Rsync mirror with a command like ``rsync --no-h --no-motd --list-only -r --exclude-from=rsync-excludes-gnome rsync://mirror.umd.edu/gnome/``.
* RubyGems: There is a `version index <https://rubygems.org/versions>`__ that lists all the gems and their versions. Or there is an API to get versions for each gem individually.
* Hackage: There is a `package index <https://hackage.haskell.org/api#core>`__. Also an RSS feed (I'm guessing it needs to set the accept header). Or there is a per-project "preferred versions" list in JSON. It is probably more efficient to use the `Git mirror <https://github.com/commercialhaskell/all-cabal-hashes>`__ though. For Stackage there are YAML files with version/build info `here <https://github.com/commercialhaskell/stackage-snapshots/>`__.
* PyPI: There are `APIs <https://warehouse.readthedocs.io/api-reference/#available-apis>`__. The RSS feed works if we can regularly check it every 20 minutes. Otherwise, besides the XML-RPC changelog API that isn't supposed to be used, the only way is to download the list of projects from the simple API and then go through and fetch the JSON data for each project. Since the requests are cached this is not too much overhead, but it can take a while for lots of projects. There is `an issue <https://github.com/pypa/warehouse/issues/347>`__ filed for a bulk API / `dump <https://github.com/pypa/warehouse/issues/1478>`__.
* CPAN: There is an RSS feed and a per-package API to get the latest version. Probably one to get all versions too.
* CRAN: There is an RSS feed and a per-package API to get all versions.
* Crates.io: There is an `index repository <https://github.com/rust-lang/crates.io-index>`__, or we could `crawl <https://crates.io/data-access>`__.
* SourceForge: There is no useful global list, but we can check each project's RSS feed to find new releases. If there are not enough files returned we can `increase the limit <https://stackoverflow.com/questions/30885561/programmatically-querying-downloadable-files-from-sourceforge>`__.
* LaunchPad, JetBrains, Drupal, Maven: There is an API to list versions for each project.
* GitHub: There is a per-project `releases API <https://developer.github.com/v4/object/release/>`__. The API is ratelimited heavily.
* GitLab, Bitbucket: There is a tags endpoint.
* Folder: We can scrape the standard default Apache directory listing
* Git/Hg/other VCS: We can fetch the tags with git/hg/etc.
* Projects not using any of the above: If there is a version number in the URL, we can scrape the download page. Otherwise, we can use HTTP caching to poll the URL. Although, for such isolated files, there is the issue of the license changing suddenly, so the download page is worth watching too.

Overall, there are only a few mechanisms:

* Feed: A way to efficiently get a list of package updates since some time (RSS feed, Git repo)
* Index: A compressed list of all the packages and their versions (Git repo, ``ls-lR``, rsync)
* Versions: For a package, a list of its available versions

For each top-level project, figuring out when/if there will be a new update is a machine learning problem. The simplest algorithm is to poll everything at a fixed interval, say daily. But most projects release a lot less frequently, and some projects (software collection, main development branches) release more frequently. If there is a push service like email we can use that, otherwise we need some sort of adaptive polling. We can model it as a homogeneous Poisson point process; then the estimate for the rate is simply the number of updates divided by the time interval we have observed. Then the time between two updates is an exponential distribution with parameter the rate, so we can poll if the probability of an update is > 50%, adjusting the 50% so we poll an average of once a day. To get even more complex, we can build a feature vector classifier to predict the time between events.

Automation
==========

Along with a Linux distribution (or any large software collection) comes the need to continuously test and update packages. An automation system (tentatively titled "Flux99") handles several tasks:
* Pulling together new changes
* Testing changes and identifying breakages
* Generating reports
* Uploading a nightly release

Since our goal is automation, we want the detection of breakages to be automated as well. Detecting breakages is an imperfect science: there are exponentially many combinations of different changes, and tests can be flaky. So in general we can only identify updates that have a high probability of causing a breakage. The problem falls under "stochastic scheduling", in particular determining which subset of changes to schedule a build for, given uncertain information about build successes/failures.

The general goal is to minimize the time/build resources needed for identifying breakages, i.e. to maximize the information gained from each build. Incremental building means that the most efficient strategy is often building in sequence, but this does not hold for larger projects where changes are almost independent.

Regarding the ordering of changes, oftentimes they are technically unordered and could be merged in any order. But an optimized order like "least likely to fail" could lead to arbitrarily long merge times for risky changes. It is simpler to do chronological order. This could be customized to prioritize hotfixes before other changes, but it is easier to set up a dedicated code path for those.

To handle breakages, there are two main strategies: marking and backouts. Both are useful; a test failure may be unimportant or outdated, suggesting the marking strategy, while backouts reject bad changes from the mainline and keep it green. Backouts are harder to compute: for :math:`n` changes, there are :math:`2^n` possible combinations to test, giving a state space of size :math:`2^{2^n}`. Meanwhile marking only has :math:`2^n` states for :math:`n` commits. Marking is run over the entire commit history, while backouts are for pending changes and only need to consider the relevant subsets of commits.

Marking
-------

For marking, we can model the test process as follows:

::

  broken = false
  for commit in commits:
    commit_type <- choice([broken ? FIXING : BREAKING, NONE], broken, commit)
    if commit_type = BREAKING:
      broken = true
    else if commit_type = FIXING:
      broken = false

    for run in runs:
      flaky <- choice([YES, NO], broken)
      if flaky = YES:
        report(!broken)
      else:
        report(broken)

The choice function can be an arbitrarily complicated function of ``commit``, but since the outcome is a random binary we can distill it down to two probabilities for each commit :math:`k`: fixing :math:`P(f_k)` and breaking :math:`P(b_k)`. We'll want complex models to predict these, like the logistic models from :cite:`najafiBisectingCommitsModeling2019` that use the list of files changed / modified components, presence of keywords in commit message, etc., or naive Bayes models that use similar factors but converge faster. Regardless, our model boils down to a hidden Markov process with two states, broken and working. Since the state space is so small we probably want to work with the second-order process, so we can easily identify breaking and fixing commits. The initial state is known to be working.

For observations, if we assume that the probability of false positive / false success :math:`P(p_k)` and false negative / false failure :math:`P(n_k)` are fixed per commit, then the probability of observing :math:`i` test failures and :math:`j` test successes (in a given/fixed order) given that the build is broken / not broken is

.. math::

  P(o_k = f^i s^j \mid r_k) = (1-P(p_k))^i P(p_k)^j

  P(o_k = f^i s^j \mid \neg r_k) = P(n_k)^i (1-P(n_k))^j

We will want to use the logit function :cite:`wikipediaLogit2020` instead of computing products of small floating point numbers. We can also use a per-run model of flakiness, e.g. based on analyzing the test logs; then each success/failure probability is calculated individually. Whatever the case, we can then use the forward-backward algorithm :cite:`wikipediaForwardBackwardAlgorithm2020` to smooth all the observations and compute the individual probabilities that each commit is broken / breaking / fixing. This can then be propagated back to compute the probability that each run is flaky. When all is said and done we end up with a table:

.. list-table::
   :header-rows: 1

   * - Change #
     - P(Broken)
     - P(Type)
     - Run #
     - P(Flaky)
     - Result
   * - 101
     - 0.02
     - Breaking 0.1, Fixing 0.2
     - 1
     - 0.01
     - Success
   * -
     -
     -
     - 2
     - 0.01
     - Success
   * -
     -
     -
     - 3
     - 0.03
     - Failure
   * - 102
     - 0.01
     - Breaking 0.1, Fixing 0.5
     - 1
     - 0.02
     - Success

Given a breakage, we can use the dependency graph traces to narrow a failure down to a specific build task, so most of the graph can be ruled out immediately and skipped during a rebuild. :cite:`ziftciWhoBrokeBuild2017`
The table treats the build as a unit; for added precision we should make one table for each failing test and a UI to aggregate them somehow. From this table, we can make decisions: reporting breakages, hiding flaky runs, blacklisting broken builds, blessing working revisions, etc. once a certainty threshold is reached.

For deciding the next build, a simple heuristic is to find the build with ``P(Broken)`` closest to 50%; but this ignores flakiness. What we want is to maximize the expected `information gain <https://en.wikipedia.org/wiki/Information_gain_in_decision_trees>`__ from a run :math:`X`.


, i.e. something like

.. math::

  H(X) = - P(x_s) \log(P(x_s)) - P(x_f) \log(P(x_f))

where :math:`x_s = 1 - x_f` is the probability that the run will succeed. To accommodate differing build costs we can simply divide by the cost; it works for Bayesian search of boxes so it probably works here.

Overall, the idea is similar to ``git bisect``'s ``min(ancestors,N-ancestors)``, but with more advanced models and using expectation instead of ``min``. To implement a full regression tool we also need to mark and handle untestable revisions, where the test is not observable due to the build being broken etc. This is fairly straightforward and amounts to doubling the state space and adding some more probability models.

Backouts
--------

For backouts, we must first decide a backout strategy. The paper :cite:`ananthanarayananKeepingMasterGreen2019` provides a real-world case study. We should maximize the number of changes included, respecting chronological order. So for ``A,B`` and ``A,C`` we should prefer the earlier change ``B``. Also, for ``A`` vs ``B,C``, to get ``B,C`` we would have to decide to test without ``A`` even though it succeeds. Since ``A`` could already been pushed to mainline this is unlikely to be the desired behavior. So the backout strategy is lexicographic preference: we write ``A,B`` and ``B,C`` as binary numbers ``110`` and ``011`` and compare them, and the higher is the chosen result.

We assume that if a build fails that adding more patches to that build will still result in a failing build; this rules out "fixing" changes where ``A`` fails but ``A,B`` succeeds because ``B`` fixed ``A``. Detecting fixing changes would require speculatively building extra changes on top of failed builds. Instead, the fixing patchset must include the broken commits as well, so we would have ``A`` failing, ``B`` succeeding, and ``A,B`` resulting in a merge conflict (because ``B`` includes the changes from ``A``). Merge conflicts can often be detected immediately without running tests, but complex failures can arise from code interactions.

We need a more complex model accounting for breakages, dependencies, conflicts, and flakiness. But we'll assume no higher-order phenomena, e.g. fixes to conflicts.

::

  breaking = []
  for c in changes:
    is_breaking <- choice([YES, NO], c)
    if is_breaking:
      breaking += c

  dependencies = {}; dependencies.default = []
  for c2 in changes:
    for c in changes:
      if c2 <= c:
        continue
      is_dependency <- choice([YES, NO], c, c2)
      if is_dependency:
        dependencies[c2] += c

  conflicts = []
  for c2 in changes:
    for c in changes:
      if c2 <= c:
        continue
      is_conflict <- choice([YES, NO], c, c2)
      if is_conflict:
        conflicts[c2] += c

  function query_run(set):
    fail_type = NONE

    for b in breaking:
      if !set.contains(b)
        continue
      fail_type = BREAKAGE

    for c in set:
      for d in dependencies[c]:
        if !set.contains(d)
          fail_type = DEPENDENCY

    for c2 in conflicts:
      for c in conflicts[c2]:
        if set.contains(c) && set.contains(c2)
          fail_type = CONFLICT

    flaky = choice([YES, NO], fail_type)
    broken = fail_type == NONE
    if flaky = YES:
      report(!broken)
    else:
      report(broken)

The size and complexity presents a challenge, but at the end of the day it's just a large Bayesian network, and we want to determine the highest-ranking success, based on the (unobserved/hidden) brokenness properties.

We can work it out for 4 commits. There are ``4+(4*3)/2*2=16`` hidden variables:

* Breaking b1, b2, b3, b4
* Conflicts c12, c13, c14, c23, c24, c34
* Dependencies d12, d13, d14, d23, d24, d34

We can work out the failure conditions for each build candidate:

1234: b1 || b2 || b3 || b4 || c12 || c13 || c14 || c23 || c24 || c34
123: b1 || b2 || b3 || c12 || c13 || c23
124: b1 || b2 || b4 || c12 || c14 || c24 || d34
12: b1 || b2 || c12
134: b1 || b3 || b4 || c13 || c14 || c34 || d23 || d24
13: b1 || b3 || c13 || d23
14: b1 || b4 || c14 || d24 || d34
1: b1
234: b2 || b3 || b4 || c23 || c24 || c34 || d12 || d13 || d14
23: b2 || b3 || c23 || d12 || d13
24: b2 || b4 || c24 || d12 || d14 || d34
2: b2 || d12
34: b3 || b4 || c34 || d13 || d14 || d23 || d24
3: b3 || d13 || d23
4: b4 || d14 || d24 || d34
empty: true

Now we write down the conditions for each set to be the best set, i.e. that it does not fail and that all higher sets do fail:

1234: !b1 && !b2 && !c12 && !b3 && !c13 && !c23 && !b4 && !c14 && !c24 && !c34
123: !b1 && !b2 && !c12 && !b3 && !c13 && !c23 && (b4 || c14 || c24 || c34)
124: !b1 && !b2 && !c12 && (b3 || c13 || c23) && !b4 && !c14 && !c24 && !d34
12: !b1 && !b2 && !c12 && (b3 || c13 || c23) && (b4 || c14 || c24 || d34)
134: !b1 && (b2 || c12) && !b3 && !c13 && !d23 && !b4 && !c14 && !d24 && !c34
13: !b1 && (b2 || c12) && !b3 && !c13 && !d23 && (b4 || c14 || d24 || c34)
14: !b1 && (b2 || c12) && (b3 || c13 || d23) && !b4 && !c14 && !d24 && !d34
1: !b1 && (b2 || c12) && (b3 || c13 || d23) && (b4 || c14 || d24 || d34)
234: b1 && !b2 && !d12 && !b3 && !d13 && !c23 && !b4 && !d14 && !c24 && !c34
23: b1 && !b2 && !d12 && !b3 && !c23 && !d13 && (b4 || d14 || c24 || c34)
24: b1 && !b2 && !d12 && (b3 || d13 || c23) && !b4 && !d14 && !c24 && !d34
2: b1 && !b2 && !d12 && (b3 || d13 || c23) && (b4 || d14 || c24 || d34)
34: b1 && (b2 || d12) && !b3 && !d13 && !d23 && !b4 && !d14 && !d24 && !c34
3: b1 && (b2 || d12) && !b3 && !d13 && !d23 && (b4 || c34 || d14 || d24)
4: b1 && (b2 || d12) && (b3 || d13 || d23) && !b4 && !d14 && !d24 && !d34
empty: b1 && (b2 || d12) && (b3 || d13 || d23) && (b4 || d14 || d24 || d34)

Each formula is in CNF and has 10 variables, 4 b variables and 6 c or d. So it is a "nice" structure.

compilation is special because incremental compilation. I compile each patch in the series one after another in the same directory, and after each compilation I zip up the files needed for testing.


I run the test that had not passed for the longest time, to increase confidence in more patches. If a test fails, I bisect to find the patch that broke it, reject the patch, and throw it out of the candidate.

When bisecting, I have to compile at lots of prefixes of the candidate, the cost of which varies significantly based on the directory it starts from. I'm regularly throwing patches out of the candidate, which requires a significant amount of compilation, as it has to recompile all patches that were after the rejected patch.
    I'm regularly adding patches to the candidate, each of which requires an incremental compilation.

unzipping only needs to be done when bisecting is required; zipping is cheap. And the testing fileset is smaller than the building fileset.

When testing a candidate, I run all tests without extending the candidate. If all the tests pass I update the state and create a new candidate containing all the new patches.

If any test fails I bisect to figure out who should be rejected, but don't reject until I've completed all tests. After identifying all failing tests, and the patch that caused each of them to fail, I throw those patches out of the candidate. I then rebuild with the revised candidate and run only those tests that failed last time around, trying to seek out tests where two patches in a candidate both broke them. I keep repeating with only the tests that failed last time, until no tests fail. Once there are no failing tests, I extend the candidate with all new patches, but do not update the state.

As a small tweak, if there are two patches in the queue from the same person, where one is a superset of the other, I ignore the subset. The idea is that if the base commit has an error I don't want to track it down twice, once to the first failing commit and then again to the second one.
Using this approach in Bake

If there is a failure when compiling, it caches that failure, and reports it to each step in the bisection, so Bake tracks down the correct root cause.


Paper
-----

$ ./flx <program executable> <input-file>
$ emacs <input-file>
$ echo "<off> <len>" >> changes.txt
$ ./flx <program executable> <input-file>

Our approach relies on recording the data and control
dependencies in a computation during the initial run by con-
structing a Concurrent Dynamic Dependence Graph (CDDG).
The CDDG tracks the input data to a program, all sub-
computations (a sub-computation is a unit of the computation
that is either reused or recomputed), the data flow between
them, and the final output. For the incremental run, a (paral-
lel) change propagation algorithm updates the output and the
CDDG by identifying sub-computations that are affected by
the changes and recomputing only those sub-computations.

At a high level, the basic approach proceeds in the following three steps:
1. Divide a computation into a set of sub-computations N.

We assume the language allows distinguishing between synchronization and ordinary (non-synchronization or data) operations. We say that two memory operations conflict if they access the same memory location (for example, variable or array element), and at least one is a write.  Data races are conflicting accesses not ordered by synchronization. To ensure that two conflicting ordinary operations do not happen simultaneously, they must be ordered by intervening synchronization operations. For example, one thread must release a lock after accessing a shared variable, and the other thread must acquire the lock before its access. In fact though our only allowed operations are fork and join, as these are sufficient to implement a build system; other synchronization primitives such as locks, barriers, etc. can be added later.

We divide a thread execution into sub-computations at the boundaries of synchronization points.
Thunks (or sub-computations). We define a thunk as the sequence of instructions executed by a thread between two
pthreads synchronization API calls, i.e. every unlock() denotes the end of a sub-computation (the unlock is included) and the beginning of the next. We model an execution of thread t as a sequence of thunks (L_t). Thunks in a thread are totally ordered based on their execution order using a monotonically increasing thunk counter (α). We refer to a thunk of thread t using the counter α as an index in the thread execution sequence (L_t), i.e., L_t [α].

This is the Release Consistency model. Under Sequential Consistency, one would have to track
individual load/store instructions as the granularity
of sub-computations, which would be prohibitively expensive.

We say that a program (on a particular input) allows a data race if it has a sequentially consistent execution (that is, a program-ordered interleaving of operations of the individual threads) in which two conflicting ordinary op-erations execute “simultaneously.”  For our purposes, two operations execute “simultaneously” if they occur next to each other in the interleaving and corre-spond to different threads. Since these operations occur adjacently in the inter-leaving, we know that they could equally well have occurred in the opposite or-der; there are no intervening operations to enforce the order.


The RC model still guarantees correctness and liveness for applications that are data-race-free.
Consequently, iThreads assumes that the programs are data-race-
free w.r.t. pthreads synchronization primitives, which is in fact mandated
in the pthread standard for correctness.

A program that does not allow a data race is said to be data-race-free. The data-race-free model guarantees sequen-tial consistency only for data-race-free programs.1,3 For programs that allow data races, the model does not provide any guarantees.

2. CDDG construction. During the initial run, record an execution trace to con-
struct a Concurrent Dynamic Dependence Graph (or
CDDG). The CDDG is a directed acyclic graph with vertices represent-
ing sub-computations (or thunks), and different types of edges to
record dependencies between thunks.
The CDDG captures a partial order O = (N, →)
among sub-computations with the following property:
given a sub-computation n ∈ N and the subset
of sub-computations M that it depends on,
i.e., M = {m ∈ N | m → n}, if the outputs of
all m ∈ M are unchanged, then n’s output is also unchanged and
we can reuse n’s memoized effect without recomputing n.
So the transitive relation determines whether a sub-computation
could be affected by an input change.

To understand the dependencies that need to be recorded to build the CDDG, we
consider incremental runs with changes either in the input
data or the thread schedule (shown in a_example.hs).  Let us assume T2 runs second, resulting in the following thread
schedule for sub-computations: T1.a → T2 .a → T2 .b.

We first consider the case of change in the input data, when the value of variable y is
changed—in this case, we need to recompute T1.a because it
reads the modified value of y. In contrast, we can still reuse
T2.a because it is independent of y and also not affected by
the writes made by T1.a. However, we might need to recompute
T2.b even though it does not directly depend on y, if
T1.a writes a different value of z.

Therefore, the CDDG needs to record data
dependencies (meaning which sub-computations write a
value that is read by another sub-computation) to determine
whether a sub-computation can be reused or if it has to be
recomputed. In particular
data dependencies are recorded implicitly in the
CDDG by recording the read and write sets: if we know what
data is read and written by each sub-computation, we can
determine whether a data dependency exists, i.e., if a sub-
computation is reading data that was modified by another
sub-computation (read-after-write data dependency).

Data-dependence edges. Data dependencies are tracked
to establish the update-use relationship between thunks.
Intuitively, such a relationship exists between two thunks
if one reads data written by the other. More formally, for a
thunk Lt [α], the read-set Lt [α].R and the write-set Lt [α].W
are the set of addresses that were read-from and written-to,
respectively, by the thread t while executing the thunk. Two
thunks L(t1 ) [α] and L(t2 ) [β] are then connected by a
• data-dependence edge iff L(t2) [β] is reachable from
L(t1 )[α] via happens-before edges and L(t1 ) [α].W ∩
L(t2 )[β].R 6= ∅.

We next consider the case of a change in the thread
schedule. In general, multi-threaded programs are non-
deterministic because the OS scheduler is free to interleave
sub-computations in different ways. As a result, a prob-
lem can arise if the initial and the incremental runs follow
different schedules. This might alter the shared state, and
therefore cause unnecessary re-computations even without
any input changes. For example if thread T1 runs after T2
(i.e., a changed thread schedule of T2 .a → T2 .b → T1 .a) then sub-
computations T1 .a and T2 .b need to be recomputed because
of the changed value of y. Therefore, the CDDG also records
the partial order happens-before/(→) between sub-
computations, according to the synchronization events.
It ensures that, given unchanged input and that all threads acquire locks in the same order as
dictated by →, all sub-computations remain unchanged.

Step #3: Change propagation.

The incremental run visits sub-computations in an order that is compatible with the recorded partial order →.
For each sub-computation, it uses the read and write sets to
determine whether part of its input was modified during the
incremental run, reusing sub-computations whose input is unchanged
and re-computing those whose input has changed.
If the read-set is modified then the sub-
computation is re-computed, otherwise we skip the execution
of the sub-computation, and directly write the memoized
value of the write-set to the address space.

4.1
 Concurrent Dynamic Dependence Graph (CDDG)
: happens-before edges
and data-dependence edges. We next explain how to derive
vertices and edges.

Happens-before edges. There are two types of happens-
before edges: control edges, which record the intra-thread
execution order; and synchronization edges, which record
explicit inter-thread synchronization events.
Control edges are simply derived by ordering thunks of the
same thread based on their execution order. Synchronization
edges are derived by modeling synchronization primitives
as acquire and release operations. In particular, during syn-
chronization, a synchronization object s is released by one
set of threads and subsequently acquired by a correspond-
ing set of threads blocked on the synchronizing object. For
example, an unlock(s) operation releases s and a corre-
sponding lock(s) operation acquires it.
Under the acquire-release relation, a release operation
happens-before the corresponding acquire operation. Given
that a thunk’s boundaries are defined at synchronization
points, the acquire and release operations also establish the
happens-before ordering between thunks of different threads.
Formally, two thunks L(t1 )[α] & L(t2 )[β] are connected by a
• control edge iff they belong to the same thread (t1 = t2 )
and L(t1 )[α] was executed immediately before L(t2 )[β];
• synchronization edge iff L(t1 ) [α] releases a synchroniza-
tion object s and L(t2 )[β] is a thunk that acquires s next.

4.2 Algorithm for the Initial Run
During the initial run, we record the execution of the program
to construct the CDDG. Algorithm 2 presents the high-level
overview of the initial run algorithm, and details of the subrou-
tines used in the algorithm are presented in Algorithm 3. The
algorithm is executed by threads in parallel. The algorithm
employs run-time techniques to derive the information needed
for the CDDG. In particular, during a thread execution, the
thread traces memory accesses on load/store instructions
(using routine onMemoryAccess()), and adds them to the
read and the write set of the executing thunk. (Our implemen-
tation, described in §5, derives the read and write sets at the
granularity of memory pages using the OS memory protec-
tion mechanism.) The thread continues to execute instructions
and perform memory tracing until a synchronization call is
made to the pthreads library. At the synchronization point,
we define the end point for the executing thunk and memoize
its end state (using routine endThunk()). Thereafter, we let
the thread perform the synchronization. Next, we start a new
thunk and repeat the process until the thread terminates.

To infer the CDDG, control and synchronization edges are
derived by ordering thunks based on the happens-before order.
To do so, we use vector clocks (C) [65] to record a partial
order that defines the happens-before relationship between
thunks during the initial run, and in the incremental run we
follow this partial order to propagate the changes. Our use
of vector clocks is motivated by its efficiency for recording a
partial order in a decentralized manner, rather than having to
serialize all synchronization events in a total order.
Our algorithm maintains one vector clock for each thread,
thunk, and synchronization object. These vector clocks are
an array of size T , where T denotes the number of threads in
the system, which are numbered from 1 to T .
Each thread t has a vector clock, called its thread clock Ct ,
to track its local logical time, which is updated at the start of
each thunk (using routine startThunk()) by setting Ct [t]
to the thunk index α. Further, each thunk Lt [α] has a thunk
clock Lt[α].C, which stores a snapshot of Ct[t] to record the
thunk’s position in the CDDG.
Finally, each synchronization object s has a synchroniza-
tion clock Cs that is used to order release and acquire op-
erations (see onSynchronization()). More precisely, if a
thread t invokes a release operation on s, then t updates Cs
to the component-wise maximum of its own thread clock Ct
and Cs. Alternatively, if t invokes an acquire operation on
s, it updates its own thread clock Ct to the component-wise
maximum of Ct and s’s synchronization clock Cs . This en-
sures that a thunk acquiring s is always ordered after the last
thunk to release s.
At the end of the initial run algorithm, the CDDG is
defined by the read/write sets and the thunk clock values
of all thunks.
4.3
 Algorithm for the Incremental Run
The incremental run algorithm takes as input the CDDG
(∀t : Lt ) and the modified input (named the dirty set M ), and
performs change propagation to update the output as well
as the CDDG for the next incremental run. As explained in
the basic change propagation algorithm (Algorithm 1), each
thread transitions through its list of thunks by following the
recorded happens-before order to either reuse or recompute
thunks. To make this algorithm work in practice, however,
we need to address the following three challenges.
(1) Missing writes. When a thunk is recomputed during the
incremental run, it may happen that the executing thread no
longer writes to a previously written location because of a
data-dependent branch. For such cases, our algorithm should
update the dirty set with the new write-set of the thunk as
well as the missing writes. These consist of the set of memory
locations that were part of the thunk’s write-set in the previous
run, but are missing in the current write-set.
(2) Stack dependencies. As mentioned previously, we trans-
parently derive read and write sets by tracking the global
memory region (heap/globals) using the OS memory protec-
tion mechanism (detailed in §5). Unfortunately, this mech-
anism is inefficient for tracking the per-thread stack region
(which usually resides in a single page storing local vari-
ables) because the stack follows a push/pop model, where the
stack is written (or gets dirty) when a call frame is pushed
or popped, even without a local variable being modified. To
avoid the overheads of tracking local variables, we do not
track the stack. Instead, we follow a conservative strategy
to capture the intra-thread data dependencies. In our design,
once a thunk is recomputed (or invalidated) in a thread, all
remaining thunks of the thread are also invalidated in order
to capture a possible change propagation via local variables.
(3) Control flow divergence. During the incremental run, it
may happen that the control flow diverges from the recorded
execution. As a result of the divergence, new thunks may be
created or existing ones may be deleted. As in the previous
challenge, the algorithm we propose takes a simple approach
of only reusing a prefix of each thread (before the control
flow diverges), and subsequently recording the new CDDG
for enabling change propagation in subsequent runs.
Details. Algorithm 4 presents the overview of the incremental
run algorithm, and details of subroutines used in the algorithm
are presented in Algorithm 5. The incremental run algorithm
allows all threads to proceed in parallel, and associates a
state with each thunk of every thread. The state of each
thunk follows a state machine (shown in Figure 4), which
enforces that each thread waits until all thunks that happened-
before its next thunk to be executed are resolved (i.e., either
recomputed or reused), and only when it is certain that reusing
memoized results is not possible will it start to re-execute
Unresolved
1
 Enabled
Pending
 2
4
 Invalid
3
5
Resolved
Resolved Reused and applied
valid
 memoized effects
Resolved
invalid
Re-executed and
modified dirty set
Figure 4. State transition for thunks during incremental run
its next thunk. In particular, the state of a thunk is either
resolved or unresolved. The state of a thunk is resolved when
the thunk has either been reused (resolved-valid) or re-
executed (resolved-invalid). Otherwise, the thunk is still
unresolved. An unresolved thunk is in one of the following
states: pending, enabled or invalid.
Initially, the state of all thunks is pending, except for
the initial thunk, which is enabled. A pending thunk is
not “ready” to be considered for re-computation or reuse.
A pending thunk of a thread is enabled (state transition
1 ) when all thunks (of any thread) that happened-before are
resolved (either resolved-valid or resolved-invalid).
To check for this condition (using routine isEnabled()),
we make use of the strong clock consistency condition [65]
650
provided by vector clocks to detect causality (a → b iff
C(a) < C(b)). In particular, we compare the recorded clock
value of the thunk against the current clock value of all
threads to check that all threads have passed the time recorded
in the thunk’s clock.
An enabled thunk transitions to invalid (state tran-
sition 2 ) if the read set of the thunk intersects with the
dirty set. Otherwise, the enabled thunk transitions to
resolved-valid (state transition 3 ), where we skip the
execution of the thunk and directly apply the memoized
write-set to the address space, including performing the syn-
chronization operation (using the resolveValid() routine).
A pending thunk transitions to invalid (state transition
4 ) if any earlier thunk of the same thread is invalid
or resolved-invalid. The invalid thunk transitions to
resolved-invalid (state transition 5 ) when the thread
re-executes the thunk and adds the write set to the dirty
set (including any missing writes). The executing thread
continues to resolve all the remaining invalid thunks to
resolved-invalid until the thread terminates. To do so,
we re-initialize the read/write sets of the new thunk to the
empty set and start the re-execution, similarly to the initial run
algorithm (using the resolveInvalid() routine). While re-
executing, the thread updates the CDDG, and also records
the state of the newly formed thunks for the next run.
5.
 Implementation
We implemented iThreads as a 32-bit dynamically link-
able shared library for the GNU/Linux OS (Figure 5).
iThreads reuses two mechanisms of the Dthreads imple-
mentation [63]: the memory subsystem (§5.1) and a custom
memory allocator (§5.4). Additionally, our implementation
also includes the iThreads memoizer, which is a stand-alone
application. We next describe the implementation in detail.
5.1
 iThreads Library: Memory Subsystem
The iThreads memory subsystem implements the RC mem-
ory model and derives per-thunk read/write sets.
Release consistency memory model. To implement the RC
memory model, iThreads converts threads into separate
processes using a previously proposed mechanism [17]. This
“thread-as-a-process” approach provides each thread with its
own private address space, and thus allows iThreads to
restrict inter-thread communication. In practice, iThreads
forks a new process on pthread create() and includes
a shared memory commit mechanism [28, 56] that enables
communication between processes at the synchronization
points, as required by the RC memory model.
At a high level, throughout the application execution,
iThreads maintains a copy of the address space contents in a
(shared) reference buffer, and it is through this buffer, with in-
strumentation provided by iThreads at the synchronization
points, that the processes transparently communicate (Fig-
ure 6). Communication between processes is implemented
Application
iThreads library
Memoizer
Recorder / Replayer
CDDG
Memory subsystem
 OS support
OS
Figure 5. iThreads implementation architecture. Shaded
boxes represent the main components of the system.
by determining the thunk write-set, as explained next, which
is then used to calculate a byte-level delta [63].
To compute the byte-level delta for each dirty page,
iThreads performs a byte-level comparison between the
dirty page and the corresponding page in the reference buffer,
and then applies atomically the deltas to the reference buffer.
In case there are concurrent writes by different processes to
the same memory location, iThreads resolves the conflict
by using a last-writer wins policy.
Furthermore, for efficiency reasons, the implementation
of the communication mechanism relies on private memory-
mapped files—this allows different processes to share phys-
ical pages until processes actually write to the pages, and
still keeps performance overheads low by virtue of the OS
copy-on-write mechanism.
Read and write set. Besides serving as the foundation for
the RC memory model, the adopted thread-as-a-process
mechanism is also essential for easily deriving per-thread
read and write sets. More specifically, iThreads uses the OS
memory protection mechanism to efficiently track the read
and write sets. In particular, iThreads renders the address
space inaccessible by invoking mprotect(PROT NONE) at
the beginning of each thunk, which ensures that a signal is
triggered the first time a page is read or written by the thunk.
Hence, within the respective signal handler, iThreads is able
to record the locations of the accesses made to memory at the
granularity of pages. Immediately after recording a memory
access, the iThreads library proceeds to reset the page
protection bits, allowing the thunk to resume the read/write
operation as soon as the handler returns. In addition, resetting
the permissions also ensures that subsequent accesses proceed
without further page faults. In this way, iThreads incurs at
most two page faults (one for reads & one for writes) for each
accessed page during a thunk execution.
5.2
 iThreads Library: Recorder and Replayer
The iThreads library executes the application in either
recording or replaying mode. We next describe the two sub-
components, recorder and replayer, that realize these modes
of execution by implementing the algorithms described in §4.
Recorder. Since iThreads reuses the Dthreads memory
subsystem, which serializes memory commit operations
from different threads, the implementation of the recording
algorithm is greatly simplified. Due to the resulting implicit
serialization of thunk boundaries, the employed thread, thunk,
651
Thread-1
 Shared
 Thread-2
private address space
 address space
 private address space
Thunk execution
Shared memory
commit
Write
Sync
Write
Sync
Thunk execution
Shared memory
commit
Thunk execution
 Thunk execution
Figure 6. Overview of the RC model implementation
and synchronization vector clocks effectively reduce to scalar
sequence numbers, which allows the recorder to simply
encode the thread schedule using thunk sequence numbers.
The recorder is further responsible for memoizing the state
of the process at the end of each thunk. To this end, using
an assembly routine, iThreads stores the register values on
the stack, takes a snapshot of the dirty pages in the address
space, and stores the snapshot in the memoizer (§5.4). In
addition, the recorder also stores the CDDG, consisting of
thunk identifiers (thread number and thunk sequence number)
and their corresponding read/write sets, to an external file.

our current implementation assumes the number
of threads in the system remains the same. However, our
approach can be extended to handle dynamically varying
number of threads by considering newly forked threads
or deleted threads as invalidated threads, where the writes
of deleted threads are handled as “missing writes”. The
happens-before relationship for dynamically varying number
of threads can be detected using interval tree clocks [11].


Replayer. Similarly to the recorder, the replayer relies on
thunk sequence numbers to enforce the recorded schedule
order. The replayer first reads the file with the input changes
and the CDDG to initialize the replay algorithm. During an
incremental run, whenever memoized thunks can be reused,
the replayer retrieves the appropriate state from the memoizer,
patches the address space and restores the state of registers.
5.3
 iThreads Library: OS Support
As practical applications depend on OS services, there are
two important aspects related to the OS that iThreads needs
to address. First, system calls are used by the application
to communicate with the rest of the system, so the effects
of system calls (on the system and application) need to be
addressed; in particular, input changes made by the user need
to be handled. Second, there are OS mechanisms that can
unnecessarily change the memory layout of the application
across runs, preventing the reuse of memoized thunks.
System calls and input changes. Since iThreads is a user-
space library running on top of an unmodified Linux kernel, it
has no access to kernel data structures. The effects of system
calls thus cannot be memoized or replayed. To support system
calls, iThreads instead considers system calls to be thunk
delimiters (in addition to synchronization calls). Hence, im-
mediately before a system call takes place, iThreads mem-
oizes the thunk state, and immediately after the system call
returns, iThreads determines whether it still can reuse the
subsequent thunks according to the replayer algorithm.
To ensure that system calls take effect (externally and inter-
nally), iThreads invokes system calls in all executions, even
during replay runs. To guarantee that effects of system calls
on the application (i.e., the return values and writes made to
the address space) are accounted for by the thunk invalidation
rules, iThreads infers the write-set of the system calls and
checks whether the write-set contents match previous runs by
leveraging knowledge of system call semantics (e.g., some
system call parameters return pointers where data is written).
An important special case is that of reading the potentially
large input to the computation (e.g., using mmap). In this
case, iThreads efficiently identifies the content that does
not match across runs by allowing the user to specify input
changes explicitly. This relies on an external file, either
written manually by users or produced by external tools, that
lists the modified offset ranges (Figure 1).
In practice, our implementation intercepts system calls
through wrappers at the level of glibc library calls.

Memory layout stability. To avoid causing unnecessary
data dependencies between threads, iThreads reuses the
custom memory allocator of Dthreads, which is based
on HeapLayer [16]. The allocator isolates allocation and
deallocation requests on a per-thread basis by dividing the
application heap into a fixed number of per-thread sub-heaps.
This ensures that the sequence of allocations in one thread
does not impact the layout of allocations in another thread,
which otherwise might trigger unnecessary re-computations.
In addition, iThreads disables Address Space Layout
Randomization (ASLR) [1], an OS feature that deliberately
randomizes the memory layout.
5.4
 iThreads Memoizer
The memoizer is responsible for storing the end state of
each thunk so that its effects can be replayed in subsequent
incremental runs. The memoizer is implemented as a separate
program that stores the memoized state in a shared memory
segment, which serves as the substrate to implement a key-
value store that is accessible by the recorder/replayer.

Metrics: work and time. We consider two types of measures,
work and time. Work refers to the total amount of computation
performed by all threads and is measured as the sum of the
total runtime of all threads. Time refers to the end-to-end
runtime to complete the parallel computation. Time savings
reflect reduced end user perceived latency, whereas work
savings reflect improved resource utilization.
Note that work speedups do not directly translate into
time speedups. This is because even if just a single thread is
affected by changes, the end-to-end runtime is still dominated
by the (slowest) invalidated thread’s execution time.
Measurements. For all measurements, each application was
executed 12 times. We exclude the lowest and highest mea-
surements, and report the average over the 10 remaining runs.

As expected, we observed that increasing the number of
threads tended to yield higher speedups. This is because,
for a fixed input size, a larger number of threads translates
to less work per thread. As a result, iThreads is forced to
recompute fewer thunks when a single input page is modified.

speedups increase with the input size due to increased work savings.
speedups decrease as larger portions of the input are changed because more threads are invalidated.

iThreads incurs additional overheads on
top of Dthreads mainly from two sources: memoization
of the intermediate address space state and read page faults
(Dthreads incurs write faults only).