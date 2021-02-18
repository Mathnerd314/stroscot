Build system
############

Stroscot's build system is called "Cot". Although build systems are often an afterthought in programming language design, they interface with the compiler in several areas, so it is better to integrate the build system into the compiler as a library. That way intermediate results such as checked/optimized functions can be stored efficiently and rebuilt only when needed. What is usually called "separate compilation" is actually incremental compilation. Compilation's goal is to produce a single executable / DLL, so these "separate" files are still part of the same assembly. Object files are simply one way of storing intermediate results so that compilation work done on the files can be shared; but it does not handle some patterns, e.g. recursive references. Also, the compiler's include-following mechanism can be tightly integrated with the build system, so that generated files can be created before they are used. Finally the compiler's source code will need a build system to manage the complexity of the build.

Overview
========

.. graphviz::

  digraph foo {
    rankdir=LR;
    {
    rank=same
    IKey [label="Input keystate"]
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
    "Rule engine" -> "Traces" [color = "black:black:black"]

    IKey -> "Changes"
    IKey2 -> "Changes"

    {"Changes"; "Traces"} -> "Incremental rule engine"
    "Incremental rule engine" -> OKey2
  }

Broadly, a build system is an execution engine combined with logic for incremental computing.
A clean build starts with inputs, which the execution engine uses to execute to completion to produce the set of outputs. For most builds, we also have reference to the previous runs of the engine. In the simplest case it is only the trace of the previous run that is available, but a caching build system with constructive traces ("build farm") can maintain many sets of traces in parallel and interpolate between them.

Logically, there is a clear separation between inputs and outputs. If the build system creates the data, it is an output, otherwise it's an input. If we modify a file during the build, e.g. a source code formatter tidying up, this is an input (the original file) and an output (the tidied file) indexed by the same key. We can always remove the confusion by backing up the input, running the build, moving the output to a different key, and restoring the input.

If an input is overwritten before any data is read, then it is not actually used and the key is an output key. Detecting this in a filesystem tracer might be tricky.

If an output is modified or deleted, the clean build semantics dictates that it will be regenerated from the inputs. But a lot of the time we don't care about most of the outputs (intermediate files) so Stroscot includes damage handling logic to compute the minimal rebuild for the desired outputs.

Design
======

It's based on iThreads (:cite:`bhatotiaIThreadsThreadingLibrary2015`), which out of a few dozen incremental computation papers seemed the most appealing. The approach supports arbitrary multithreaded shared-memory programs, and hence imposes no requirements on the overall flow of the build. Instead, it requires structuring the build into fine-grained units of computation called thunks. Thunks are the smallest addressable unit of execution and constrain the amount of re-use that an incremental build can achieve. A thunk reads and writes the shared memory as it pleases, so long as it avoids data races with other concurrently executing thunks, and then calls a synchronization operation. Synchronization operations enforce control dependencies and structure the flow of time in the program. Stroscot implements a sequencing operation based on Shake's concurrency support as well as acquire/release lock from the paper. It is relatively simple to add new concurrency operations so long as they are of type ``a -> IO ()``.

Two thunks conflict if they access the same key, and at least one is a write. Data races are conflicting accesses not ordered by synchronization. To ensure that two conflicting ordinary operations do not happen simultaneously, they must be ordered by intervening synchronization operations. For example, one thunk could write a variable and then release a lock; other thunks must acquire the lock before accessing the variable, so that there is a locking operation intervening. If the build is not data race free it might  execution orders produce the same results. This is not checked thoroughly.

Thunk names
===========

Thunk naming adds some complexity to the implementation of a build system, as the thunk names also affect the way computations can be re-used. From the implementation side there is no restriction on the names; they can be any sequence of bytes. iThreads uses a simple "thread # thunk #" scheme, which assumes a fixed number of long-running threads and invalidates all of the thunks in a thread after a modified thunk. A scheme similar to ``make`` uses filenames; for each file f there are two thunks "run f" and "exec f". The "run f" just does ``Sequence [subtargets,[exec f]]`` while "exec f" runs the commands that generate f. But with fine-grained dependency tracking we can track each command separately - we could use thunk names like "exec f step #" but this leads to invalidating later thunks. Using names like "exec f step cmd" requires a lot of boilerplate names to be written out. The ideal solution is probably some form of structural hashing.

Also, in a dynamic build, a direct file action map like this is not always available, and so the naming scheme must be relaxed to allow dependencies on things that aren't files. For example, we may have one command that generates two files; so long as we use a consistent thunk name for this command there is no issue. For another example, we may have include headers that are picked up in a search path directory listing. To deal with this directly, we would need to introduce build logic into the search mechanism and run dependencies when seeing ``#include``. But a phase separation handles it fine with minimal changes - we generate the files first and then call the compiler, filling in the build dependencies from an output list of used headers. In this case we would need thunks for each phase.


Model
=====

To reason about the code we need a pencil-and-paper model of how it works. First we have thunk IDs (``tid`` s); these come from the program and are quoted strings "abc". For key names we use unquoted strings xyz and for key values integers 123; these are only compared for equality (often they are modification times). Then for the traces we use a tabular format to record the reads, writes, and synchronization operations. We might have databases from multiple runs available, so there is also a "machine" column, but this is the same for all rows in a single trace so it is omitted. An example database based on the example in :cite:`shalBuildSystemRules2009` might be

.. raw:: html

  <style>
    .shal-trace-example th p {
      margin-bottom: 0px;
    }
    .shal-trace-example tr:nth-child(1) td,
    .shal-trace-example tr:nth-child(2) td,
    .shal-trace-example tr:nth-child(3) td,
    .shal-trace-example tr:nth-child(7) td,
    .shal-trace-example tr:nth-child(11) td,
    .shal-trace-example tr:nth-child(15) td
    {
      border-bottom-color: #b1b4b5;
    }
  </style>

.. csv-table::
  :header: tid,op,rest
  :quote: ^
  :widths: auto
  :class: shal-trace-example

  "run prog",sync,^Sequence [["run main","run parse"],["ld"]]^
  "run main",sync,^Sequence [["yacc"],["cc main"]]^
  "run parse",sync,^Sequence [["yacc"],["cc parse"]]^
  "yacc",read,parse.y 1
  "yacc",write,parse.h 2
  "yacc",write,parse.c 2
  "yacc",sync,Die
  "cc main",read,main.c 1
  "cc main",read,parse.h 2
  "cc main",write,main.o 3
  "cc main",sync,Die
  "cc parse",read,parse.c 2
  "cc parse",read,parse.h 2
  "cc parse",write,parse.o 3
  "cc parse",sync,Die
  "ld",read,parse.o 3
  "ld",read,main.o 3
  "ld",write,prog 4
  "ld",sync,Die

One way to understand the database is to draw it in a graph:

.. graphviz::

    digraph multi {
        rankdir=RL
        node [shape="rect",fontsize=20]
        "main.c", "main.o", "prog", "parse.o", "parse.h", "parse.c", "parse.y" [shape="circle"]

        // run prog = ExecAfter [run main,run parse] ld
        "run prog" -> "run main" [style=dotted, color=grey,penwidth=3]
        "run prog" -> "run parse" [style=dotted, color=grey,penwidth=3]
        "run prog" -> ld [color=grey,penwidth=3]
        // run main = ExecAfter [yacc] "cc main"
        "run main" -> "yacc" [style=dotted, color=grey,penwidth=3]
        "run main" -> "cc main" [color=grey,penwidth=3]
        // run parse = ExecAfter [yacc] "cc parse"
        "run parse" -> "yacc" [style=dotted, color=grey,penwidth=3]
        "run parse" -> "cc parse" [color=grey,penwidth=3]

        "cc main" -> "main.c"
        "cc main" -> "parse.h"
        "main.o" -> "cc main" [color=blue]

        "ld" -> "main.o"
        "ld" -> "parse.o"
        "prog" -> "ld" [color=blue]

        "cc parse" -> "parse.h"
        "cc parse" -> "parse.c"
        "parse.o" -> "cc parse" [color=blue]

        "yacc" -> "parse.y"
        "parse.h" -> "yacc" [color=blue]
        "parse.c" -> "yacc" [color=blue]

    }

Square nodes represent thunks while circular nodes are keys (files). Black lines are reads. Blue lines are writes. Dotted gray lines are sequenced to execute before solid gray lines. Overall, the graph structure is very similar to Pluto's two-level graph, but the control structure is more complex - Pluto simply has build-require, while Cot has various synchronization operations.

Then during an incremental run we start with a list of changed keys and their values; this is allowed to contain unmodified keys, so generating this list may be as simple as calculating the state of all keys and saying they all might be modified, or it may be a more precise list from a filesystem watcher or similar API. The keys can also include volatile information such as FTP server listings or stdin.

.. csv-table::
  :header: key,value
  :quote: ^
  :widths: auto

  parse.y,1
  main.c,5

Here main.c's modification time has been updated. We start from the top and load "run prog"; there are no changed inputs (or indeed any inputs), so we skip execution of the thunk, perform the record write operations to the key state, and execute the synchronization operation, which loads "run main". "run main" loads "yacc" which has not changed, so control returns to "run main" and "cc main" is loaded. "cc main"'s inputs have changed, so we run it, producing an updated main.o. Meanwhile "run parse" and "cc parse" have been loaded with no changes. Control returns to "run prog" and "ld" is executed as its inputs have changed, building the final executable "prog".

Thunk state
===========

Thunk state is a bit tricky to define precisely. So let's work it out.

First we define execution state. A thunk is enabled once a synchronization operation requests to execute the thunk. A thunk is resolved once it is enabled and its synchronization operation has begun execution. So a thunk starts out disabled, becomes enabled, and then is resolved.

If a thunk is never enabled, then in a clean build the thunk would not be executed at all. There are two possibilities:

* unused: The thunk is not referenced by any trace or by the current build. Example: almost any arbitrary thunk id
* stale: The thunk is referenced by some trace but is not enabled anytime in the current build. Example: control flow change

If a thunk is enabled, then we can consider the available traces and compare them with the keystate at the point the thunk is enabled. There is one trivial possibility:

* new: The thunk is not referenced by any trace but has been enabled in the current build. Examples: control flow change, clean build

When we have at least one trace, things get more interesting. A trace is valid if all of its recorded reads match the state of the build. There is also relevancy; a key is relevant if it might have changed this run, relative to the previous run.

* dirty: There are traces but no valid trace. Example: input change
* clean: There is a valid trace where all recorded writes match the state on disk. Example: A thunk is always clean immediately after it is executed, since running a thunk records its trace.
* damaged: There is at least one valid trace but no valid trace has its recorded writes matching the state on disk. Examples: shallow build, external modification, overwritten output

After resolving the thunk, it can only be clean or damaged; the clean state may have been achieve by substitution, reuse, or rebuilding, while the damaged state can only be from a damaged thunk passing the no-future-use check.

In a cloud build setting we have one more state to handle constructive traces. A constructive trace stores the full value for each key and allows fetching the output files without running the build.

* substitutable: There is a valid constructive trace.

A substitutable thunk can be clean or damaged but not dirty. So in total we have 8 states: unused, stale, new, dirty, clean-nonsubstitutable, clean-substitutable, damaged-nonsubstitutable, and damaged-substitutable. It's a lot, but Cot deals with a lot of functionality.

Simulation
==========

It's possible for a thunk to be handled in several ways: leave damaged/clean, rebuild, or substitute with one of the applicable versions. These also have different costs: leaving things alone is free, substituting costs some amount of network bandwidth time / decompression, while rebuilding's cost is unknown but can be estimated from other builds. But to figure out the least-cost action overall we need a global view of the build. Dmaaged thunks can only be left alone if they are not need during the rest of the build, i.e. no thunk reading the damaged data is rebuilding. Substitutions from different sources may be incompatible (e.g. GHC used to produce `randomized symbols names <https://gitlab.haskell.org/ghc/ghc/-/issues/4012>`__), so picking the version is not as simple as first from a list.

The problem is NP-hard since we can encode 3-SAT in the substitution versions :cite:`coxVersionSAT2016`. But of course, it it's that hard, we might as well use a SAT solver. In particular we can encode it as an instance of partial weighted MaxSAT. First we have a lot of hard constraints:

* each thunk can be left alone, substituted, or built, and we can only do one: ``t_leave + t_rebuild + t_v1 + ... + t_vn = 1`` (this is a pseudo-Boolean constraint that be easily encoded)
* compatibility on the read/write values, ``t_vj -> (s_vx or s_vy or ...)``, where t reads a value that s writes and x,y, etc. are the versions that are compatible (this leaves out rebuilding, which could generate identical data)
* a conservative assumption that rebuilding changes all outputs to new versions, ``s_rebuild -> t_rebuild`` where t reads from what s writes
* a requirement that rebuilds not use damaged data, ``t_rebuild -> not s_leave``, where s is damaged and t reads from s.

Then we have soft constraints for each variable weighted with the cost of using that option.

To generate these constraints, Cot walks through the build graph and maintains a multi-valued state. So it would look like ``Key i -> [Value 1 S_1, Value 1 S_2, Value 2 S_3, Damaged S_leave]``. Then for each thunk (visited in normal traversal order) Cot generates the constraints for each possibility. Then Cot updates the possible values for the keys it writes.

To deal with these constraints we need a MaxSAT solver - we can write a custom one or interface with an existing one. Using an off-the-shelf solver might save some effort, but there is significant overhead in serializing the constraints to an external solver, and this overhead can be avoided by using a native solver. The native solver will probably be naive and not have the finely tuned optimizations or heuristics of the off-the-shelf solvers, but most package version problems are very simple to solve. It'll be easier to build the project with a native solver because all of the code will be in the same language (Haskell or Stroscot). In Cox's list of package managers (at the end of :cite:`coxVersionSAT2016`), the split is 9-5 in favor of a native solver (although 3 of the native-solver package managers allow using an external solver with an option, so more like 9-8). Overall it seems writing a native solver is the best course of action. But we don't have to start from scratch as there is a Haskell MaxSAT solver in toysolver on Hackage.

Wanted files
------------

When using Cot as a package manager rather than a build system, we have lots of produced files that aren't used by anything. Since Cot doesn't see any users of the files it'll leave them as damaged (unmaterialized) and not download them. So at the end of the build process we'd run special thunks that simply read in a bunch of files, to ensure that the files are up-to-date and available for use. These thunks are always out of date, which can be though of as having a special wanted key that always compares unequal. In the end these special thunks are actually the packages.

We could also add functionality to force re-executing specific damaged thunks.

Restarting
----------

The constraint model is only an approximation of the truth, in particular it doesn't cover a newly-executed thunk that adds a dependency on damaged data. The restarting strategy restarts build execution from the damaged thunk on detection of a read, which allows the build to continue if there is an unexpected dependency on damaged data. It requires traversal of the build graph to reconstruct the keystate at the point of re-execution, and all the work done after the point of re-execution is thrown away, so its efficiency isn't optimal. In particular it is possible to re-execute a unit several times, in the case where we execute a unit B, then go back and re-execute a unit A due to damage, then have to execute B another time due to A changing C changing input to B.

Graph pruning
=============

Pruning the build graph as pioneered by Tup can result in a big speedup, only having to load/inspect the part of the build graph that's necessary. But it requires some auxiliary data structures and careful record-keeping in order to look up the pieces efficiently.

We start with a change list, i.e. things that might have changed since our last build. The prototypical example is a list of changed files from a file-watching daemon. The alternative is scanning all the files for changes on startup. This can take several minutes with a hashing algorithm or a few seconds with modtimes.

First we process the change list into a list of possibly-changed keys. There are many various options (digest, modtime, etc.), so we need a hash table that maps key writes to all the thunks with key reads, really a filename->(set of thunk) table.

So in our build example, we would go from "main.c" to "cc main". Next we want load the other thunks "run main", "run prog", "ld". The first two are the ancestors of the thunk; we have to load the parent to see its synchronization operation and thus the order of execution. But we don't have to load any children of the parents.  So we just need a thunk->(thunk parents) map to find all the parents.

We also have to load "ld"; this is done by looking up the writes of "cc main" in the filename->thunk table. We need to load thunks that read from the writes during execution, in case they are different from the recorded writes.

Note that we'll always load the initial thunk, because we load the chain of parents. So after everything is loaded, execution can start from the initial thunk as normal, no need for a topological sort like in Tup. The difference is that we may have unloaded thunks as children; we do not want to execute these. But to keep the keystate consistent we need to be able to modify the keystate as though they were executed. In particular for each thunk we need the list of all the writes performed by the thunk and its children. But the thunk itself already stores the writes in its ThunkRecord; so computing the total writes is a matter of combination, ``Total = Thunk // Union(Children)``, where ``//`` is record update from Nix. These write lists can be precomputed during the initial run. Storing them efficiently with fast access is a little tricky since there is a lot of copying in the lists. For now I'll store the full write list for each thunk, compressed, but there is probably a persistent data structure (`tree <https://en.wikipedia.org/wiki/Self-balancing_binary_search_tree>`__\ ?) that can efficiently re-use the data from other thunks while maintaining performance. At the other extreme we can just regenerate all the write lists by walking the thunk records, so these write lists can be cached and expired using LRU or something.
We also need to store the list of acquire/release lock operations, but most programs don't use locks so this will be small.

The write lists can also be used as an incomplete check for data races; if after executing a thunk A, A has read a key from the global/shared keystate with a value different from the local keystate passed into the thunk (state passed into the parent thunk P // modifications of P // modifications synced in from synchronization operation of P), then a thunk not in the execution history of A must have modified the key - since this execution could have been delayed by the scheduler, it is a read-write data race. Similarly in the union of the children, if there are differing values among the children then there is a write-write data race.

Anyway, the recorded state also records if the key is damaged and the thunk that regenerates it. So we can use this during our damage simulation to load in damaged thunks when referenced and re-run them if necessary.

Pipeline
========

We could think of an option to allowing a "what-if" query, "what would the output be if the task that made the output generated this". But the semantics is murky - to preserve the modification, we must disable all of the tasks that would modify the generated file, which if they write other files means that the build will contain out-of-date files.

Cleaning
========

When we re-execute a thunk, it is a good idea to restore the state of the outputs of the thunk to their original state (typically deleting them). Also at the end of the run we should garbage collect any unused thunks from the old run by deleting their outputs. Also in (hopefully rare) cases we want to delete all the outputs regardless of status.

-c, --clean, --remove

    Clean up by removing the selected targets, well as any files or directories associated with a selected target through calls to the Clean function. Will not remove any targets which are marked for preservation through calls to the NoClean function.

* Before running a task, we clean up old build results, if any, i.e. delete all provided keys (outputs) that are still present. After running a task we store its (keyed) outputs with either verifying or constructive traces.
* To prune the store (which is a bad idea if there are multiple configurations that build different subsets), we can do as above and also load all the subtasks of present tasks. Then anything not loaded is not needed and its files etc. can be deleted.


Exceptions
==========

Shake tries to be exception-safe, handling GHC's broken `asynchronous exception system <https://www.fpcomplete.com/blog/2018/04/async-exception-handling-haskell/>`__. The system is broken because it is so complicated that nobody can agree on the desired behavior / correct form of even simple examples. The prototypical example of using it is `bracket <https://hackage.haskell.org/package/unliftio-0.2.13.1/docs/UnliftIO-Exception.html#v:bracket>`__:

::

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

Here we use 4 operations: mask, try, ``uninterruptibleMask_``, throwIO. mask shields the cleanup action from being attacked by asynchronous exceptions, allowing exceptions inside restore. try catches exceptions and allows cleanup to occur. ``uninterruptibleMask_`` blocks interrupts from interrupting the after handler. Finally throwIO rethrows the exception, so that any exception inside the after handler will be swallowed.

Apparently, though, nobody can agree on whether the after handle should run with an uninterruptible mask.

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

If the key contents are large, we can intern it - journal an association "#5 = x", then writes as "write key xyz is interned to #5 = ...", and reads as "read key xyz from intern #5". We can't use the key itself as #n because there might be multiple writes to the key.

The simplest example of an in-memory key is the command line arguments; we can store the full initial command line, and then have a thunk that parses the command line and writes various option keys. Another example is versioning keys. The initial thunk writes a key for each thunk with the compiled-in version, ``write (Version abc) v2.3``. Then each thunk reads its version and this read is stored in the thunk record, causing rebuilds when the version is changed.

Files
-----

Files are a little trickier because storing the whole contents of the file in the journal is infeasible. Instead we journal a proxy of the contents, stored in-memory. So writes look like "write file f with proxy p" and reads are "read file f with proxy p". We assume that there aren't any untracked writes during the build so the reads can be recorded using the in-memory value of p calculated from the writes.

trivial proxy
  Sometimes we want to ignore the file contents and always/never do an action. In such a case we can use a trivial proxy. There are two types, "always rebuild" and "never rebuild". In the never case, the rebuild can still be triggered by a different file.

dirty bit
   The idea of a dirty bit is to have one piece of information per key, saying whether the key is dirty or clean. In the initial state all keys are clean. If a thunk executes, all its writes set the keys to dirty. A thunk that reads a dirty key must also execute. But if all read keys are clean, the thunk does not need to be rerun.

version number
  For toolchains in small projects, the version number from running ``gcc -V`` etc. is often sufficient. Although modtime is more robust, it's worth listing this as an example of a custom file modification detector.

file size/permissions/inode number
  Checking the file size is fast and cheap as it's stored in every filesystem. This catches most changed files, but is incomplete since a modification may keep the same file size. In most cases it isn't necessary to track this as modification time alone is sufficient. File permissions can also be relevant, if they are changed from the default.

modtime/device/inode number
  As opposed to make's simple "is-newer" comparison, storing the full mtime value is pretty accurate. mtime changes at least as often as the content hash changes. There is a small risk that a file archiver or inaccurate clock will set the timestamp to collide with the old one and the change won't be detected. The device/inode number detects replaced files, e.g. if you ``mv`` a file onto another one. The real disadvantage is over-rebuilding, due to ``touch`` and similar. ctime and atime update even more frequently than mtime, so they don't help. btime / creation time might be useful, in a manner similar to inode number. Simply checking all the modtimes sequentially is very efficient due to filesystem caching and it can be made even more efficient with various tricks (parallel threads, maybe grouping by directory).

digest
  A digest computed from the contents. There is a remote risk that the file will change without its digest changing due to a collision, but otherwise this detects changes accurately. The disadvantage of digests is that they are somewhat slow to compute, requiring a full scan of the file. But various virtual filesystems store precalculated file checksums, in which case those would be better to use than mtime. There are fast hash algorithms like `xxHash <https://cyan4973.github.io/xxHash/>`__ that have throughput faster than RAM, so the main bottleneck is the I/O. Looking at the `benchmark <https://github.com/Cyan4973/xxHash/wiki/Performance-comparison>`__, and fruitlessly googling around to find other hashes not listed there (fnv1, murmurhash, siphash), it seems xxHash3 / xxHash128 are the fastest. But, if we are going to share the files over a network then one of the SHA's or BLAKE3 might be better to prevent file-replacement attacks. There is also the Linux Kernel Crypto API using AF_ALG but it seems to be slower than doing it in user-space.

watcher/change journal
  We can run a filesystem watching service like Watchman, on Windows use the `USN journal <https://en.wikipedia.org/wiki/USN_Journal>`__, strace all running programs, or redirect filesystem operations through a FUSE vfs. In each case, we get a list (journal) of all changes since some arbitrary starting point. If the journal covers all of the time since the last build, we have a full list of changes and don't need anything else; otherwise we need to supplement it with one of the other methods.

We can construct modes from the various combinations:

::

  digest Files change when digest changes.
  digest-and ModtimeAndDigest Files change when modtime and digest change.
  digest-and-input ModtimeAndDigestInput Files change on modtime (and digest for inputs).
  digest-or ModtimeOrDigest Files change when modtime or digest change.
  digest-not Modtime Files change when modtime changes.

* digest-only, if modification times on your file system are missing or don't update on changes.
* modtime-only, if your timestamps change mostly in sync with the file content
* modtime-then-digest, if you could use modtimes but want to avoid spurious rebuilds. In particular git touches a lot of files when switching branches, vim copies over the file so its inode changes frequently, and scripts/you can write identical files.
* modtime-then-some-digest, skipping digests for generated files as they're large and change with almost every rebuild. Generated file modtimes can be kept constant by writing to a temporary file and only replacing the output if it's different.
* watcher-only, if your watcher runs continuously or if you delete all files after every run
* modtime-then-watcher, if your watcher's change journal is incomplete
* modtime-then-watcher-then-digest, to get the fastest file tracking and fewest rebuilds

‘-L’
‘--check-symlink-times’

    On systems that support symbolic links, this option causes make to consider the timestamps on any symbolic links in addition to the timestamp on the file referenced by those links. When this option is provided, the most recent timestamp among the file and the symbolic links is taken as the modification time for this target file.

io_uring
~~~~~~~~

It's a little overkill, but the io_uring interface on Linux allows batching up calls asynchronously, which can speed up stat() and thus modtime reading by 20%. For hashing parallelism is likely counterproductive, as xxHash is I/O bound and parallelism turns sequential reads into random reads.

Access Tracing
~~~~~~~~~~~~~~

Specifying a lot of file read/write dependencies manually is tedious and error-prone, although writing a small script from scratch is not too difficult. So instead we want to use automatic tracing. There are various tracing methods:
* library preloading with fsatrace: fails on static linking, Go programs, and Mac system binaries
* ptrace with BigBro-fsatrace: Linux-only at present, might work on Windows/Mac eventually.
* chroot with FUSE: mount real system at ``/real-system/``, FUSE system with all files ``/x`` as symlinks to ``/real-system/x``. The program shouldn't access ``/real-system/`` directly. Handles all programs, even forking/multiprocess programs like make, and gives build system the abilities to hide new files and generate files on-demand. Requires Linux + root.
* modtime checking: a little slow but useful if none of the other methods work. Doesn't work multithreaded.

When we get back file paths from these tracers, they are usually absolute paths, or paths relative to the working directory. But we want standardized paths - if the build doesn't need to be copied/moved, then e.g. the home directory path should be omitted. Rattle's solution of named relative directories seems reasonable. Basically, if we have ``NAME=/x/y`` and a path ``/x/y/z`` then we shorten it to ``$NAME/z``, similarly expanding the name, and we sort the list of names to do this efficiently (or maybe use a tree?).

If the list of files read/written is static and won't ever change, another idea is to save space in the build journal by skipping writing the trace and instead writing a note that says "compute the trace using the static list". But a lot of file dependencies are dynamic (e.g. header files), so it's not clear how often this could be used. Also if the file list changes between build system versions then the database will be subtly corrupted.

Network
-------

Often we wish to fetch data from over the network. There are a few common protocols:

* HTTP downloads: we can use wget, curl, aria2, or a custom library. The caching headers (Last-Modified & ETag) are important for re-using old downloads.
* FTP: this can be treated similarly to HTTP
* Git, Bittorrent, IPFS: these are content-addressed stores so keeping track of the hash is sufficient

A more complex example is deploying a container to AWS. The inputs are: all the configuration details for the host, the container image itself, and secret credential information. The output is a running instance or else a long log file / error message. But the running instance cannot be checksummed, so we must use some proxy criterion - the easiest is to redeploy if any inputs have changed, but we could also use a script to interrogate the running instance over the network.

If there are multiple containers that depend on each other, we have to encode the restarting behavior somehow. The easiest is probably to write a single script that takes all the configuration and starts up the containers in order, but this duplicates the build system task scheduling logic.

Damage scratchwork
------------------

* neural net that does 5 runs/subtasks which write back to file

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


Options
=======

Files :: FilePath
 ^ Defaults to @.shake@. The directory used for storing Shake metadata files.
   All metadata files will be named @'shakeFiles'\/.shake./file-name/@, for some @/file-name/@.
   If the 'shakeFiles' directory does not exist it will be created.
   If set to @\"\/dev\/null\"@ then no shakeFiles are read or written (even on Windows).
Flush :: Maybe Seconds
 ^ Defaults to @'Just' 10@. How often to flush Shake metadata files in seconds, or 'Nothing' to never flush explicitly.
   It is possible that on abnormal termination (not Haskell exceptions) any rules that completed in the last
   'shakeFlush' seconds will be lost.

    ""  ["flush"] (reqIntArg 1 "flush" "N" (\i s -> s{shakeFlush=Just i})) "Flush metadata every N seconds."
    ""  ["never-flush"] (noArg $ \s -> s{shakeFlush=Nothing}) "Never explicitly flush metadata."
    "m" ["metadata"] (reqArg "PREFIX" $ \x s -> s{shakeFiles=x}) "Prefix for storing metadata files."
    ""  ["rule-version"] (reqArg "VERSION" $ \x s -> s{shakeVersion=x}) "Version of the build rules."
    ""  ["no-rule-version"] (noArg $ \s -> s{shakeVersionIgnore=True}) "Ignore the build rules version."

Cloud build
-----------

Share :: Maybe FilePath
 ^ Defaults to 'Nothing'. Whether to use and store outputs in a shared directory.
Cloud :: [String]
 ^ Defaults to @[]@. Cloud servers to talk to forming a shared cache.
Symlink :: Bool
 ^ Defaults to @False@. Use symlinks for 'shakeShare' if they are available.
   If this setting is @True@ (even if symlinks are not available) then files will be
   made read-only to avoid inadvertantly poisoning the shared cache.
   Note the links are actually hard links, not symlinks.

 --cache-debug=file

    Write debug information about derived-file caching to the specified file. If file is a hyphen (-), the debug information is printed to the standard output. The printed messages describe what signature-file names are being looked for in, retrieved from, or written to the derived-file cache specified by CacheDir.

--cache-disable, --no-cache

    Disable derived-file caching. scons will neither retrieve files from the cache nor copy files to the cache. This option can be used to temporarily disable the cache without modifying the build scripts.

--cache-force, --cache-populate

    When using CacheDir, populate a derived-file cache by copying any already-existing, up-to-date derived files to the cache, in addition to files built by this invocation. This is useful to populate a new cache with all the current derived files, or to add to the cache any derived files recently built with caching disabled via the --cache-disable option.

--cache-readonly

    Use the derived-file cache, if enabled, to retrieve files, but do not not update the cache with any files actually built during this invocation.

--cache-show

    When using a derived-file cache and retrieving a file from it, show the command that would have been executed to build the file. Without this option, scons reports "Retrieved 'file' from cache.". This allows producing consistent output for build logs, regardless of whether a target file was rebuilt or retrieved from the cache.

,yes $ Option ""  ["cloud"] (reqArg "URL" $ \x s -> s{shakeCloud=shakeCloud s ++ [x]}) "HTTP server providing a cloud cache."
""  ["share"] (optArg "DIRECTORY" $ \x s -> s{shakeShare=Just $ fromMaybe "" x, shakeChange=ensureHash $ shakeChange s}) "Shared cache location."
,hide $ Option ""  ["share-list"] (noArg ([ShareList], ensureShare)) "List the shared cache files."
,hide $ Option ""  ["share-sanity"] (noArg ([ShareSanity], ensureShare)) "Sanity check the shared cache files."
,hide $ Option ""  ["share-remove"] (OptArg (\x -> Right ([ShareRemove $ fromMaybe "**" x], ensureShare)) "SUBSTRING") "Remove the shared cache keys."
""  ["share-copy"] (noArg $ \s -> s{shakeSymlink=False}) "Copy files into the cache."
""  ["share-symlink"] (noArg $ \s -> s{shakeSymlink=True}) "Symlink files into the cache."

Remote Builds

Nix supports remote builds, where a local Nix installation can forward Nix builds to other machines. This allows multiple builds to be performed in parallel and allows Nix to perform multi-platform builds in a semi-transparent way. For instance, if you perform a build for a x86_64-darwin on an i686-linux machine, Nix can automatically forward the build to a x86_64-darwin machine, if available.

To forward a build to a remote machine, it’s required that the remote machine is accessible via SSH and that it has Nix installed. You can test whether connecting to the remote Nix instance works, e.g.

$ nix ping-store --store ssh://mac

will try to connect to the machine named mac. It is possible to specify an SSH identity file as part of the remote store URI, e.g.

$ nix ping-store --store ssh://mac?ssh-key=/home/alice/my-key

Since builds should be non-interactive, the key should not have a passphrase. Alternatively, you can load identities ahead of time into ssh-agent or gpg-agent.

If you get the error

bash: nix-store: command not found
error: cannot connect to 'mac'

then you need to ensure that the PATH of non-interactive login shells contains Nix.
Warning: If you are building via the Nix daemon, it is the Nix daemon user account (that is, root) that should have SSH access to the remote machine. If you can’t or don’t want to configure root to be able to access to remote machine, you can use a private Nix store instead by passing e.g. --store ~/my-nix.

The list of remote machines can be specified on the command line or in the Nix configuration file. The former is convenient for testing. For example, the following command allows you to build a derivation for x86_64-darwin on a Linux machine:

.. code-block:: shell-session

  $ uname
  Linux

  $ nix build \
    '(with import <nixpkgs> { system = "x86_64-darwin"; }; runCommand "foo" {} "uname > $out")' \
    --builders 'ssh://mac x86_64-darwin'
  [1/0/1 built, 0.0 MiB DL] building foo on ssh://mac

  $ cat ./result
  Darwin

It is possible to specify multiple builders separated by a semicolon or a newline, e.g.

  --builders 'ssh://mac x86_64-darwin ; ssh://beastie x86_64-freebsd'

Each machine specification consists of the following elements, separated by spaces. Only the first element is required. To leave a field at its default, set it to -.

    The URI of the remote store in the format ssh://[username@]hostname, e.g. ssh://nix@mac or ssh://mac. For backward compatibility, ssh:// may be omitted. The hostname may be an alias defined in your ~/.ssh/config.

    A comma-separated list of Nix platform type identifiers, such as x86_64-darwin. It is possible for a machine to support multiple platform types, e.g., i686-linux,x86_64-linux. If omitted, this defaults to the local platform type.

    The SSH identity file to be used to log in to the remote machine. If omitted, SSH will use its regular identities.

    The maximum number of builds that Nix will execute in parallel on the machine. Typically this should be equal to the number of CPU cores. For instance, the machine itchy in the example will execute up to 8 builds in parallel.

    The “speed factor”, indicating the relative speed of the machine. If there are multiple machines of the right type, Nix will prefer the fastest, taking load into account.

    A comma-separated list of supported features. If a derivation has the requiredSystemFeatures attribute, then Nix will only perform the derivation on a machine that has the specified features. For instance, the attribute

    requiredSystemFeatures = [ "kvm" ];

    will cause the build to be performed on a machine that has the kvm feature.

    A comma-separated list of mandatory features. A machine will only be used to build a derivation if all of the machine’s mandatory features appear in the derivation’s requiredSystemFeatures attribute..

For example, the machine specification

nix@scratchy.labs.cs.uu.nl  i686-linux      /home/nix/.ssh/id_scratchy_auto        8 1 kvm
nix@itchy.labs.cs.uu.nl     i686-linux      /home/nix/.ssh/id_scratchy_auto        8 2
nix@poochie.labs.cs.uu.nl   i686-linux      /home/nix/.ssh/id_scratchy_auto        1 2 kvm benchmark

specifies several machines that can perform i686-linux builds. However, poochie will only do builds that have the attribute

requiredSystemFeatures = [ "benchmark" ];

or

requiredSystemFeatures = [ "benchmark" "kvm" ];

itchy cannot do builds that require kvm, but scratchy does support such builds. For regular builds, itchy will be preferred over scratchy because it has a higher speed factor.

Remote builders can also be configured in nix.conf, e.g.

builders = ssh://mac x86_64-darwin ; ssh://beastie x86_64-freebsd

Finally, remote builders can be configured in a separate configuration file included in builders via the syntax @file. For example,

builders = @/etc/nix/machines

causes the list of machines in /etc/nix/machines to be included. (This is the default.)

builders-use-substitutes

    If set to true, Nix will instruct remote build machines to use their own binary substitutes if available. In practical terms, this means that remote hosts will fetch as many build dependencies as possible from their own substitutes (e.g, from cache.nixos.org), instead of waiting for this host to upload them all. This can drastically reduce build times if the network connection between this computer and the remote build host is slow. Defaults to false.

To build only on remote builders and disable building on the local machine, you can use the option --max-jobs 0.

Not relevant to Stroscot
------------------------

‘-e’
‘--environment-overrides’

    Give variables taken from the environment precedence over variables from makefiles. See Variables from the Environment.

‘-E string’
‘--eval=string’

    Evaluate string as makefile syntax. This is a command-line version of the eval function (see Eval Function). The evaluation is performed after the default rules and variables have been defined, but before any makefiles are read.

‘-f file’
‘--file=file’
‘--makefile=file’

    Read the file named file as a makefile. See Writing Makefiles.

‘-I dir’
‘--include-dir=dir’

    Specifies a directory dir to search for included makefiles. See Including Other Makefiles. If several ‘-I’ options are used to specify several directories, the directories are searched in the order specified.

‘-r’
‘--no-builtin-rules’

    Eliminate use of the built-in implicit rules (see Using Implicit Rules). You can still define your own by writing pattern rules (see Defining and Redefining Pattern Rules). The ‘-r’ option also clears out the default list of suffixes for suffix rules (see Old-Fashioned Suffix Rules). But you can still define your own suffixes with a rule for .SUFFIXES, and then define your own suffix rules. Note that only rules are affected by the -r option; default variables remain in effect (see Variables Used by Implicit Rules); see the ‘-R’ option below.

‘-R’
‘--no-builtin-variables’

    Eliminate use of the built-in rule-specific variables (see Variables Used by Implicit Rules). You can still define your own, of course. The ‘-R’ option also automatically enables the ‘-r’ option (see above), since it doesn’t make sense to have implicit rules without any definitions for the variables that they use.

Debugging
---------

ninja subtools:
::

    browse  browse dependency graph in a web browser
     clean  clean built files
  commands  list all commands required to rebuild given targets
      deps  show dependencies stored in the deps log
     graph  output graphviz dot file for targets
     query  show inputs/outputs for a path
   targets  list targets by their rule or depth in the DAG
    compdb  dump JSON compilation database to stdout
 recompact  recompacts ninja-internal data structures
    restat  restats all outputs in the build log
     rules  list all rules
 cleandead  clean built files that are no longer produced by the manifest

demo"] (noArg [Demo]) "Run in demo mode."
sleep"] (noArg [Sleep]) "Sleep for a second before building."
exception"] (noArg [Exception]) "Throw exceptions from the top-level build function, instead of printing them and exiting with an error code."
numeric-version"] (noArg [NumericVersion]) "Print just the version number and exit."

StorageLog :: Bool
 ^ Defaults to 'False'. Write a message to @'shakeFiles'\/.shake.storage.log@ whenever a storage event happens which may impact
   on the current stored progress. Examples include database version number changes, database compaction or corrupt files.

    "r" ["report","profile"] (optArg "FILE" $ \x s -> s{shakeReport=shakeReport s ++ [fromMaybe "report.html" x]}) "Write out profiling information [to report.html]."
    ""  ["no-reports"] (noArg $ \s -> s{shakeReport=[]}) "Turn off --report."

--no-build
  Load all the database files but stop before executing the initial thunk and don't build anything.

    "l" ["lint"] (noArg $ \s -> s{shakeLint=Just LintBasic}) "Perform limited validation after the run."
    ""  ["lint-watch"] (reqArg "PATTERN" $ \x s -> s{shakeLintWatch=shakeLintWatch s ++ [x]}) "Error if any of the patterns are created (expensive)."
    ""  ["lint-fsatrace"] (optArg "DIR" $ \x s -> s{shakeLint=Just LintFSATrace, shakeLintInside=shakeLintInside s ++ [fromMaybe "." x]}) "Use fsatrace to do validation [in current dir]."
    ""  ["lint-ignore"] (reqArg "PATTERN" $ \x s -> s{shakeLintIgnore=shakeLintIgnore s ++ [x]}) "Ignore any lint errors in these patterns."
    ""  ["no-lint"] (noArg $ \s -> s{shakeLint=Nothing}) "Turn off --lint."
    ""  ["live"] (optArg "FILE" $ \x s -> s{shakeLiveFiles=shakeLiveFiles s ++ [fromMaybe "live.txt" x]}) "List the files that are live [to live.txt]."

Lint :: Maybe Lint
 ^ Defaults to 'Nothing'. Perform sanity checks during building, see 'Lint' for details.
LintInside :: [FilePath]
 ^ Directories in which the files will be tracked by the linter.
LintIgnore :: [FilePattern]
 ^ File patterns which are ignored from linter tracking, a bit like calling 'Development.Shake.trackAllow' in every rule.
LintWatch :: [FilePattern]
 ^ File patterns whose modification causes an error. Raises an error even if 'shakeLint' is 'Nothing'.
CreationCheck :: Bool
 ^ Default to 'True'. After running a rule to create a file, is it an error if the file does not exist.
   Provided for compatibility with @make@ and @ninja@ (which have ugly file creation semantics).
NeedDirectory :: Bool
 ^ Defaults to @False@. Is depending on a directory an error (default), or it is permitted with
   undefined results. Provided for compatibility with @ninja@.
VersionIgnore :: Bool
 ^ Defaults to 'False'. Ignore any differences in 'shakeVersion'.

dupbuild={err,warn}  multiple build lines for one target
phonycycle={err,warn}  phony build statement references itself

An issue is "time travel", a thunk reading a file from the previous build that hasn't yet been generated in this build. Technically, our notion of consistency is based on a "clean build", with the filesystem initialized to source files and all generated files deleted. For true replication, when re-building a task T, we would have to delete all the build files generated by tasks depending on T, in case T accidentally read "from the future". But this is more easily detected after-the-fact, particularly in the case of parallel builds.

Shake features a built in "lint" features to check the build system is well formed. To run use build --lint. You are likely to catch more lint violations if you first build clean. The lint features are listed in this document. There is a performance penalty for building with --lint, but it is typically small.
* Detects changing the current directory, typically with setCurrentDirectory. You should never change the current directory within the build system as multiple rules running at the same time share the current directory. You can still run ``cmd_`` calls in different directories using the Cwd argument.
* Changing outputs after building. Detects if any files have changed after Shake has built them. There are a couple of causes for seeing this error:

    If there is a rule producing foo.o, but another rule also modifies foo.o.
    If you are on a file system where files change modification time after a while. A standard example would be an NFS drive where the underlying network file system stores modification times to second-level resolution, but the in-memory cache keeps them precisely.
    If you modify the build sources while running a build.

A consequence of this lint triggering would be that a subsequent build would do additional work, as it spots modifications.

* trackRead/trackWrite assert various invariants about what files can be written where. Mainly

    You can only read a file that is either your dependency, or a transitive dependency.

Additionally, you can ignore certain missing rules with --lint-ignore=PATTERN. In general all files passed to trackRead or trackWrite are expected to be relative to the current directory, so --lint-ignore patterns should match those relative paths.

Using fsatrace you can augment command line programs (called with cmd or command) to automatically track which files they read and write, which turn into trackRead and trackWrite calls. To enable this feature pass --lint-fsatrace=DIR passing the directories you want to lint. Passing --lint-fsatrace is equivalent to --lint-fsatrace=. - namely only lint the current directory.

This feature requires fsatrace to be on the $PATH, as documented on the homepage. If you are using Windows, you can download a binary release here.

LiveFiles :: [FilePath]
 ^ Default to @[]@. After the build system completes, write a list of all files which were /live/ in that run,
   i.e. those which Shake checked were valid or rebuilt. Produces best answers if nothing rebuilds.
Report :: [FilePath]
 ^ Defaults to @[]@. Write a profiling report to a file, showing which rules rebuilt,
   why, and how much time they took. Useful for improving the speed of your build systems.
   If the file extension is @.json@ it will write JSON data; if @.js@ it will write Javascript;
   if @.trace@ it will write trace events (load into @about:\/\/tracing@ in Chrome);
   otherwise it will write HTML.
Progress :: IO Progress -> IO ()
 ^ Defaults to no action. A function called when the build starts, allowing progress to be reported.
   The function is called on a separate thread, and that thread is killed when the build completes.
   For applications that want to display progress messages, 'progressSimple' is often sufficient, but more advanced
   users should look at the 'Progress' data type.
Verbosity :: Verbosity
 ^ Defaults to 'Info'. What level of messages should be printed out.
Output :: Verbosity -> String -> IO ()
 ^ Defaults to writing using 'putStrLn'. A function called to output messages from Shake, along with the 'Verbosity' at
   which that message should be printed. This function will be called atomically from all other 'shakeOutput' functions.
   The 'Verbosity' will always be greater than or higher than 'shakeVerbosity'.
Trace :: String -> String -> Bool -> IO ()
 ^ Defaults to doing nothing.
   Called for each call of 'Development.Shake.traced', with the key, the command and 'True' for starting, 'False' for stopping.

    ,extr $ Option "v" ["version"] (noArg [Version]) "Print the version number and exit."
    ,extr $ Option "w" ["print-directory"] (noArg [PrintDirectory True]) "Print the current directory."
    ,extr $ Option ""  ["no-print-directory"] (noArg [PrintDirectory False]) "Turn off -w, even if it was turned on implicitly."
    ""  ["storage"] (noArg $ \s -> s{shakeStorageLog=True}) "Write a storage log."
    "d" ["debug"] (optArg "FILE" $ \x s -> s{shakeVerbosity=Diagnostic, shakeOutput=outputDebug (shakeOutput s) x}) "Print lots of debugging information."
    "V" ["verbose","trace"] (noArg $ \s -> s{shakeVerbosity=move (shakeVerbosity s) succ}) "Print more (pass repeatedly for even more)."
    "q" ["quiet"] (noArg $ \s -> s{shakeVerbosity=move (shakeVerbosity s) pred}) "Print less (pass repeatedly for even less)."
    ,both $ Option "p" ["progress"] (progress $ optArgInt 1 "progress" "N" $ \i s -> s{shakeProgress=prog $ fromMaybe 5 i}) "Show progress messages [every N secs, default 5]."
    ""  ["no-progress"] (noArg $ \s -> s{shakeProgress=const $ pure ()}) "Don't show progress messages."
    ,extr $ Option ""  ["no-time"] (noArg [NoTime]) "Don't print build time."
    ""  ["timings"] (noArg $ \s -> s{shakeTimings=True}) "Print phase timings."
Timings :: Bool
 ^ Defaults to 'False'. Print timing information for each stage at the end.
    "s" ["silent"] (noArg $ \s -> s{shakeVerbosity=Silent}) "Don't print anything."

Silent
  Don't print any messages.
Error
  Only print error messages.
Warn
  Print errors and warnings.
Info
  Print errors, warnings and # command-name (for file-name) when running a traced command.
Verbose
  Print errors, warnings, full command lines when running a command or cmd command and status messages when starting a rule.
Diagnostic
  Print messages for virtually everything (mostly for debugging).

‘--trace’

    Show tracing information for make execution. Prints the entire recipe to be executed, even for recipes that are normally silent (due to .SILENT or ‘@’). Also prints the makefile name and line number where the recipe was defined, and information on why the target is being rebuilt.

Metrics: work and time. We consider two types of measures,
work and time. Work refers to the total amount of computation
performed by all threads and is measured as the sum of the
total runtime of all threads. Time refers to the end-to-end
runtime to complete the parallel computation. Time savings
reflect reduced end user perceived latency, whereas work
savings reflect improved resource utilization.

 --debug=type[,type...]

    Debug the build process. type specifies the kind of debugging info to emit. Multiple types may be specified, separated by commas. The following entries show the recognized types:

    action-timestamps

        Prints additional time profiling information. For each command, shows the absolute start and end times. This may be useful in debugging parallel builds. Implies the --debug=time option.

        Available since scons 3.1.

    count

        Print how many objects are created of the various classes used internally by SCons before and after reading the SConscript files and before and after building targets. This is not supported when SCons is executed with the Python -O (optimized) option or when the SCons modules have been compiled with optimization (that is, when executing from ``*.pyo`` files).

    duplicate

        Print a line for each unlink/relink (or copy) of a variant file from its source file. Includes debugging info for unlinking stale variant files, as well as unlinking old targets before building them.

    explain

        Print an explanation of why scons is deciding to (re-)build the targets it selects for building.

    findlibs

        Instruct the scanner that searches for libraries to print a message about each potential library name it is searching for, and about the actual libraries it finds.

    includes

        Print the include tree after each top-level target is built. This is generally used to find out what files are included by the sources of a given derived file:

        $ scons --debug=includes foo.o

    memoizer

        Prints a summary of hits and misses using the Memoizer, an internal subsystem that counts how often SCons uses cached values in memory instead of recomputing them each time they're needed.

    memory

        Prints how much memory SCons uses before and after reading the SConscript files and before and after building targets.

    objects

        Prints a list of the various objects of the various classes used internally by SCons.

    pdb

        Re-run scons under the control of the pdb Python debugger.

    prepare

        Print a line each time any target (internal or external) is prepared for building. scons prints this for each target it considers, even if that target is up to date (see also --debug=explain). This can help debug problems with targets that aren't being built; it shows whether scons is at least considering them or not.

    presub

        Print the raw command line used to build each target before the construction environment variables are substituted. Also shows which targets are being built by this command. Output looks something like this:

::

        $ scons --debug=presub
        Building myprog.o with action(s):
          $SHCC $SHCFLAGS $SHCCFLAGS $CPPFLAGS $_CPPINCFLAGS -c -o $TARGET $SOURCES
        ...

    stacktrace

        Prints an internal Python stack trace when encountering an otherwise unexplained error.

    time

        Prints various time profiling information:

            The time spent executing each individual build command

            The total build time (time SCons ran from beginning to end)

            The total time spent reading and executing SConscript files

            The total time SCons itself spent running (that is, not counting reading and executing SConscript files)

            The total time spent executing all build commands

            The elapsed wall-clock time spent executing those build commands

            The time spent processing each file passed to the SConscript function

        (When scons is executed without the -j option, the elapsed wall-clock time will typically be slightly longer than the total time spent executing all the build commands, due to the SCons processing that takes place in between executing each command. When scons is executed with the -j option, and your build configuration allows good parallelization, the elapsed wall-clock time should be significantly smaller than the total time spent executing all the build commands, since multiple build commands and intervening SCons processing should take place in parallel.)

‘--debug[=options]’

    Print debugging information in addition to normal processing. Various levels and types of output can be chosen. With no arguments, print the “basic” level of debugging. Possible arguments are below; only the first character is considered, and values must be comma- or space-separated.

.. code-block:: none

    a (all)
        All types of debugging output are enabled. This is equivalent to using ‘-d’.
    b (basic)
        Basic debugging prints each target that was found to be out-of-date, and whether the build was successful or not.
    v (verbose)
        A level above ‘basic’; includes messages about which makefiles were parsed, prerequisites that did not need to be rebuilt, etc. This option also enables ‘basic’ messages.
    i (implicit)
        Prints messages describing the implicit rule searches for each target. This option also enables ‘basic’ messages.
    j (jobs)
        Prints messages giving details on the invocation of specific sub-commands.
    m (makefile)
        By default, the above messages are not enabled while trying to remake the makefiles. This option enables messages while rebuilding makefiles, too. Note that the ‘all’ option does enable this option. This option also enables ‘basic’ messages.
    stats        print operation counts/timing info
    explain      explain what caused a command to execute
      n (none)
        Disable all debugging currently enabled. If additional debugging flags are encountered after this they will still take effect.


--taskmastertrace=file

    Prints trace information to the specified file about how the internal Taskmaster object evaluates and controls the order in which Nodes are built. A file name of - may be used to specify the standard output.

--tree=type[,type...]

    Prints a tree of the dependencies after each top-level target is built. This prints out some or all of the tree, in various formats, depending on the type specified:

    all

        Print the entire dependency tree after each top-level target is built. This prints out the complete dependency tree, including implicit dependencies and ignored dependencies.

    derived

        Restricts the tree output to only derived (target) files, not source files.

    linedraw

        Draw the tree output using Unicode line-drawing characters instead of plain ASCII text. This option acts as a modifier to the selected type(s). If specified alone, without any type, it behaves as if all had been specified.

        Available since scons 4.0.

    status

        Prints status information for each displayed node.

    prune

        Prunes the tree to avoid repeating dependency information for nodes that have already been displayed. Any node that has already been displayed will have its name printed in [square brackets], as an indication that the dependencies for that node can be found by searching for the relevant output higher up in the tree.

    Multiple type choices may be specified, separated by commas:

    # Prints only derived files, with status information:
    scons --tree=derived,status

    # Prints all dependencies of target, with status information
    # and pruning dependencies of already-visited Nodes:
    scons --tree=all,prune,status target


‘-h’
‘--help’

    Remind you of the options that make understands and then exit.

‘-p’
‘--print-data-base’

    Print the data base (rules and variable values) that results from reading the makefiles; then execute as usual or as otherwise specified. This also prints the version information given by the ‘-v’ switch (see below). To print the data base without trying to remake any files, use ‘make -qp’. To print the data base of predefined rules and variables, use ‘make -p -f /dev/null’. The data base output contains file name and line number information for recipe and variable definitions, so it can be a useful debugging tool in complex environments.

‘-v’
‘--version’

    Print the version of the make program plus a copyright, a list of authors, and a notice that there is no warranty; then exit.

‘-w’
‘--print-directory’
‘--no-print-directory’

showing each directory as make starts processing it and as make finishes processing it. For example, if ‘make -w’ is run in the directory /u/gnu/make, make will print lines of the form:

::

  make: Entering directory `/u/gnu/make'.
  ...
  make: Leaving directory `/u/gnu/make'.

In make this option improves the output of several levels of recursive make invocations. In Cot it is only useful for tracking down commands which change the current directory; the current directory should not be changed except with ``-C``.

‘--warn-undefined-variables’

    Issue a warning message whenever make sees a reference to an undefined variable. This can be helpful when you are trying to debug makefiles which use variables in complex ways.


--warn=type, --warn=no-type

    Enable or disable (with the no- prefix) warnings. type specifies the type of warnings to be enabled or disabled:

    all

        All warnings.

    cache-version

        Warnings about the derived-file cache directory specified by CacheDir not using the latest configuration information. These warnings are enabled by default.

    cache-write-error

        Warnings about errors trying to write a copy of a built file to a specified derived-file cache specified by CacheDir. These warnings are disabled by default.

    corrupt-sconsign

        Warnings about unfamiliar signature data in .sconsign files. These warnings are enabled by default.

    dependency

        Warnings about dependencies. These warnings are disabled by default.

    deprecated

        Warnings about use of currently deprecated features. These warnings are enabled by default. Not all deprecation warnings can be disabled with the --warn=no-deprecated option as some deprecated features which are late in the deprecation cycle may have been designated as mandatory warnings, and these will still display. Warnings for certain deprecated features may also be enabled or disabled individually; see below.

    duplicate-environment

        Warnings about attempts to specify a build of a target with two different construction environments that use the same action. These warnings are enabled by default.

    fortran-cxx-mix

        Warnings about linking Fortran and C++ object files in a single executable, which can yield unpredictable behavior with some compilers.

    future-deprecated

        Warnings about features that will be deprecated in the future. Such warnings are disabled by default. Enabling future deprecation warnings is recommended for projects that redistribute SCons configurations for other users to build, so that the project can be warned as soon as possible about to-be-deprecated features that may require changes to the configuration.

    link

        Warnings about link steps.

    misleading-keywords

        Warnings about the use of two commonly misspelled keywords targets and sources to Builder calls. The correct spelling is the singular form, even though target and source can themselves refer to lists of names or nodes.

    missing-sconscript

        Warnings about missing SConscript files. These warnings are enabled by default.

    no-object-count

        Warnings about the --debug=object feature not working when scons is run with the Python -O option or from optimized Python (.pyo) modules.

    no-parallel-support

        Warnings about the version of Python not being able to support parallel builds when the -j option is used. These warnings are enabled by default.

    python-version

        Warnings about running SCons with a deprecated version of Python. These warnings are enabled by default.

    reserved-variable

        Warnings about attempts to set the reserved construction variable names $CHANGED_SOURCES, $CHANGED_TARGETS, $TARGET, $TARGETS, $SOURCE, $SOURCES, $UNCHANGED_SOURCES or $UNCHANGED_TARGETS. These warnings are disabled by default.

    stack-size

        Warnings about requests to set the stack size that could not be honored. These warnings are enabled by default.

    target_not_build

        Warnings about a build rule not building the expected targets. These warnings are disabled by default.

Parallel Execution
------------------
--random

    Build dependencies in a random order. This is useful when building multiple trees simultaneously with caching enabled, to prevent multiple builds from simultaneously trying to build or retrieve the same target files.

‘-j [jobs]’
‘--jobs[=jobs]’

  Specifies the number of recipes (jobs) to run simultaneously. With no argument, make runs as many recipes simultaneously as possible. If there is more than one ‘-j’ option, the last one is effective. See Parallel Execution, for more information on how recipes are run. Note that this option is ignored on MS-DOS.
  Defaults to @1@. Maximum number of rules to run in parallel, similar to @make --jobs=/N/@.
  For many build systems, a number equal to or slightly less than the number of physical processors
  works well. Use @0@ to match the detected number of processors (when @0@, 'getShakeOptions' will
  return the number of threads used).

‘-l [load]’
‘--load-average[=load]’
‘--max-load[=load]’

    Specifies that no new recipes should be started if there are other recipes running and the load average is at least load (a floating-point number). With no argument, removes a previous load limit.

GNU make knows how to execute several recipes at once. Normally, make will execute only one recipe at a time, waiting for it to finish before executing the next. However, the ‘-j’ or ‘--jobs’ option tells make to execute many recipes simultaneously. You can inhibit parallelism in a particular makefile with the .NOTPARALLEL pseudo-target (see Special Built-in Target Names).

On MS-DOS, the ‘-j’ option has no effect, since that system doesn’t support multi-processing.

If the ‘-j’ option is followed by an integer, this is the number of recipes to execute at once; this is called the number of job slots. If there is nothing looking like an integer after the ‘-j’ option, there is no limit on the number of job slots. The default number of job slots is one, which means serial execution (one thing at a time).

Handling recursive make invocations raises issues for parallel execution. For more information on this, see Communicating Options to a Sub-make.

If a recipe fails (is killed by a signal or exits with a nonzero status), and errors are not ignored for that recipe (see Errors in Recipes), the remaining recipe lines to remake the same target will not be run. If a recipe fails and the ‘-k’ or ‘--keep-going’ option was not given (see Summary of Options), make aborts execution. If make terminates for any reason (including a signal) with child processes running, it waits for them to finish before actually exiting.

When the system is heavily loaded, you will probably want to run fewer jobs than when it is lightly loaded. You can use the ‘-l’ option to tell make to limit the number of jobs to run at once, based on the load average. The ‘-l’ or ‘--max-load’ option is followed by a floating-point number. For example,

-l 2.5

will not let make start more than one job if the load average is above 2.5. The ‘-l’ option with no following number removes the load limit, if one was given with a previous ‘-l’ option.

More precisely, when make goes to start up a job, and it already has at least one job running, it checks the current load average; if it is not lower than the limit given with ‘-l’, make waits until the load average goes below that limit, or until all the other jobs finish.

By default, there is no load limit.

Output control
--------------

 --interactive

    Starts SCons in interactive mode. The SConscript files are read once and a scons>>> prompt is printed. Targets may now be rebuilt by typing commands at interactive prompt without having to re-read the SConscript files and re-initialize the dependency graph from scratch.

    SCons interactive mode supports the following commands:

    build [OPTIONS] [TARGETS] ...

        Builds the specified TARGETS (and their dependencies) with the specified SCons command-line OPTIONS. b and scons are synonyms for build.

        The following SCons command-line options affect the build command:

        --cache-debug=FILE
        --cache-disable, --no-cache
        --cache-force, --cache-populate
        --cache-readonly
        --cache-show
        --debug=TYPE
        -i, --ignore-errors
        -j N, --jobs=N
        -k, --keep-going
        -n, --no-exec, --just-print, --dry-run, --recon
        -Q
        -s, --silent, --quiet
        --taskmastertrace=FILE
        --tree=OPTIONS

        Any other SCons command-line options that are specified do not cause errors but have no effect on the build command (mainly because they affect how the SConscript files are read, which only happens once at the beginning of interactive mode).

    clean [OPTIONS] [TARGETS] ...

        Cleans the specified TARGETS (and their dependencies) with the specified OPTIONS. c is a synonym. This command is itself a synonym for build --clean

    exit

        Exits SCons interactive mode. You can also exit by terminating input (Ctrl+D UNIX or Linux systems, (Ctrl+Z on Windows systems).

    help [COMMAND]

        Provides a help message about the commands available in SCons interactive mode. If COMMAND is specified, h and ? are synonyms.

    shell [COMMANDLINE]

        Executes the specified COMMANDLINE in a subshell. If no COMMANDLINE is specified, executes the interactive command interpreter specified in the SHELL environment variable (on UNIX and Linux systems) or the COMSPEC environment variable (on Windows systems). sh and ! are synonyms.

    version

        Prints SCons version information.

    An empty line repeats the last typed command. Command-line editing can be used if the readline module is available.

::

    $ scons --interactive
    scons: Reading SConscript files ...
    scons: done reading SConscript files.
    scons>>> build -n prog
    scons>>> exit

Abbreviations :: [(String,String)]
 ^ Defaults to @[]@. A list of substrings that should be abbreviated in status messages, and their corresponding abbreviation.
   Commonly used to replace the long paths (e.g. @.make\/i586-linux-gcc\/output@) with an abbreviation (e.g. @$OUT@).
Color :: Bool
 ^ Defaults to 'False'. Whether to colorize the output.
    [opts $ Option "a" ["abbrev"] (reqArgPair "abbrev" "FULL=SHORT" $ \a s -> s{shakeAbbreviations=shakeAbbreviations s ++ [a]}) "Use abbreviation in status messages."
    ""  ["color","colour"] (noArg $ \s -> s{shakeColor=True}) "Colorize the output."
    ""  ["no-color","no-colour"] (noArg $ \s -> s{shakeColor=False}) "Don't colorize the output."
    ,extr $ Option ""  ["compact"] (optArgAuto "auto" "yes|no|auto" $ \x -> [Compact x]) "Use a compact Bazel/Buck style output."

LineBuffering :: Bool
 ^ Defaults to 'True'. Change 'stdout' and 'stderr' to line buffering while running Shake.

‘-O[type]’
‘--output-sync[=type]’

    Ensure that the complete output from each recipe is printed in one uninterrupted sequence. This option is only useful when using the --jobs option to run multiple recipes simultaneously (see Parallel Execution) Without this option output will be displayed as it is generated by the recipes.

    With no type or the type ‘target’, output from the entire recipe of each target is grouped together. With the type ‘line’, output from each line in the recipe is grouped together. With the type ‘recurse’, the output from an entire recursive make is grouped together. With the type ‘none’, no output synchronization is performed.



When running several recipes in parallel the output from each recipe appears as soon as it is generated, with the result that messages from different recipes may be interspersed, sometimes even appearing on the same line. This can make reading the output very difficult.

To avoid this you can use the ‘--output-sync’ (‘-O’) option. This option instructs make to save the output from the commands it invokes and print it all once the commands are completed. Additionally, if there are multiple recursive make invocations running in parallel, they will communicate so that only one of them is generating output at a time.

If working directory printing is enabled (see The ‘--print-directory’ Option), the enter/leave messages are printed around each output grouping. If you prefer not to see these messages add the ‘--no-print-directory’ option to MAKEFLAGS.

There are four levels of granularity when synchronizing output, specified by giving an argument to the option (e.g., ‘-Oline’ or ‘--output-sync=recurse’).

none

    This is the default: all output is sent directly as it is generated and no synchronization is performed.

line

    Output from each individual line of the recipe is grouped and printed as soon as that line is complete. If a recipe consists of multiple lines, they may be interspersed with lines from other recipes.

target

    Output from the entire recipe for each target is grouped and printed once the target is complete. This is the default if the --output-sync or -O option is given with no argument.

recurse

    Output from each recursive invocation of make is grouped and printed once the recursive invocation is complete.

Regardless of the mode chosen, the total build time will be the same. The only difference is in how the output appears.

The ‘target’ and ‘recurse’ modes both collect the output of the entire recipe of a target and display it uninterrupted when the recipe completes. The difference between them is in how recipes that contain recursive invocations of make are treated (see Recursive Use of make). For all recipes which have no recursive lines, the ‘target’ and ‘recurse’ modes behave identically.

If the ‘recurse’ mode is chosen, recipes that contain recursive make invocations are treated the same as other targets: the output from the recipe, including the output from the recursive make, is saved and printed after the entire recipe is complete. This ensures output from all the targets built by a given recursive make instance are grouped together, which may make the output easier to understand. However it also leads to long periods of time during the build where no output is seen, followed by large bursts of output. If you are not watching the build as it proceeds, but instead viewing a log of the build after the fact, this may be the best option for you.

If you are watching the output, the long gaps of quiet during the build can be frustrating. The ‘target’ output synchronization mode detects when make is going to be invoked recursively, using the standard methods, and it will not synchronize the output of those lines. The recursive make will perform the synchronization for its targets and the output from each will be displayed immediately when it completes. Be aware that output from recursive lines of the recipe are not synchronized (for example if the recursive line prints a message before running make, that message will not be synchronized).

The ‘line’ mode can be useful for front-ends that are watching the output of make to track when recipes are started and completed.

Some programs invoked by make may behave differently if they determine they’re writing output to a terminal versus a file (often described as “interactive” vs. “non-interactive” modes). For example, many programs that can display colorized output will not do so if they determine they are not writing to a terminal. If your makefile invokes a program like this then using the output synchronization options will cause the program to believe it’s running in “non-interactive” mode even though the output will ultimately go to the terminal.

Command Options
---------------

CommandOptions :: [CmdOption]
 ^ Defaults to @[]@. Additional options to be passed to all command invocations.

Cwd FilePath -- Change the current directory of the spawned process. By default uses the parent process's current directory. If multiple options are specified, each is interpreted relative to the previous one: ``[Cwd "/", Cwd "etc"]`` is equivalent to ``[Cwd "/etc"]``.

‘-C dir’ ‘--directory=dir’
  A global version of Cwd that runs at the beginning. You should never change the current directory of the parent process after the build starts as multiple thunks running at the same time share the current directory.

Env [(String,String)] -- ^ Replace the environment block in the spawned process. By default uses this processes environment.
AddEnv String String -- ^ Add an environment variable in the child process.
RemEnv String -- ^ Remove an environment variable from the child process.
AddPath [String] [String] -- ^ Add some items to the prefix and suffix of the @$PATH@ variable.

Stdin String -- ^ Given as the @stdin@ of the spawned process. By default the @stdin@ is inherited.
StdinBS LBS.ByteString -- ^ Given as the @stdin@ of the spawned process.
FileStdin FilePath -- ^ Take the @stdin@ from a file.
InheritStdin -- ^ Cause the stdin from the parent to be inherited. Might also require NoProcessGroup on Linux. Ignored if you explicitly pass a stdin.

Two processes cannot both take input from the same device at the same time. To make sure that only one recipe tries to take input from the terminal at once, make will invalidate the standard input streams of all but one running recipe. If another recipe attempts to read from standard input it will usually incur a fatal error (a ‘Broken pipe’ signal).

It is unpredictable which recipe will have a valid standard input stream (which will come from the terminal, or wherever you redirect the standard input of make). The first recipe run will always get it first, and the first recipe started after that one finishes will get it next, and so on.

WithStdout Bool -- ^ Should I include the @stdout@ in the exception if the command fails? Defaults to 'False'.
WithStderr Bool -- ^ Should I include the @stderr@ in the exception if the command fails? Defaults to 'True'.
EchoStdout Bool -- ^ Should I echo the @stdout@? Defaults to 'True' unless a 'Stdout' result is required or you use 'FileStdout'.
EchoStderr Bool -- ^ Should I echo the @stderr@? Defaults to 'True' unless a 'Stderr' result is required or you use 'FileStderr'.
FileStdout FilePath -- ^ Should I put the @stdout@ to a file.
FileStderr FilePath -- ^ Should I put the @stderr@ to a file.

BinaryPipes -- ^ Treat the @stdin@\/@stdout@\/@stderr@ messages as binary. By default 'String' results use text encoding and 'ByteString' results use binary encoding.
CloseFileHandles -- ^ Before starting the command in the child process, close all file handles except stdin, stdout, stderr in the child process. Uses @close_fds@ from package process and comes with the same caveats, i.e. runtime is linear with the maximum number of open file handles (@RLIMIT_NOFILE@, see @man 2 getrlimit@ on Linux).

-- | Collect the @stdout@ of the process.
--   If used, the @stdout@ will not be echoed to the terminal, unless you include 'EchoStdout'.
--   The value type may be either 'String', or either lazy or strict 'ByteString'.
--
--   Note that most programs end their output with a trailing newline, so calling
--   @ghc --numeric-version@ will result in 'Stdout' of @\"6.8.3\\n\"@. If you want to automatically
--   trim the resulting string, see 'StdoutTrim'.
newtype Stdout a = Stdout {fromStdout :: a}

-- | Like 'Stdout' but remove all leading and trailing whitespaces.
newtype StdoutTrim a = StdoutTrim {fromStdoutTrim :: a}

-- | Collect the @stderr@ of the process.
--   If used, the @stderr@ will not be echoed to the terminal, unless you include 'EchoStderr'.
newtype Stderr a = Stderr {fromStderr :: a}

-- | Collect the @stdout@ and @stderr@ of the process.
--   If used, the @stderr@ and @stdout@ will not be echoed to the terminal, unless you include 'EchoStdout' and 'EchoStderr'.
newtype Stdouterr a = Stdouterr {fromStdouterr :: a}

-- | Collect the 'ExitCode' of the process.
newtype Exit = Exit {fromExit :: ExitCode}

-- | Collect the 'ProcessHandle' of the process.
--   If you do collect the process handle, the command will run asyncronously and the call to 'cmd' \/ 'command'
--   will return as soon as the process is spawned. Any 'Stdout' \/ 'Stderr' captures will return empty strings.
newtype Process = Process {fromProcess :: ProcessHandle}

-- | Collect the time taken to execute the process. Can be used in conjunction with 'CmdLine' to
--   write helper functions that print out the time of a result.
--
-- @
-- timer :: ('CmdResult' r, MonadIO m) => (forall r . 'CmdResult' r => m r) -> m r
-- timer act = do
--     ('CmdTime' t, 'CmdLine' x, r) <- act
--     liftIO $ putStrLn $ \"Command \" ++ x ++ \" took \" ++ show t ++ \" seconds\"
--     pure r
--
-- run :: IO ()
-- run = timer $ 'cmd' \"ghc --version\"
-- @
newtype CmdTime = CmdTime {fromCmdTime :: Double}

-- | Collect the command line used for the process. This command line will be approximate -
--   suitable for user diagnostics, but not for direct execution.
newtype CmdLine = CmdLine {fromCmdLine :: String}

Shell -- ^ Pass the command to the shell without escaping - any arguments will be joined with spaces. By default arguments are escaped properly.
Traced String -- ^ Name to use with 'traced', or @\"\"@ for no tracing. By default traces using the name of the executable.
Timeout Double -- ^ Abort the computation after N seconds, will raise a failure exit code. Calls 'interruptProcessGroupOf' and 'terminateProcess', but may sometimes fail to abort the process and not timeout.
AutoDeps -- ^ Compute dependencies automatically. Only works if 'shakeLintInside' has been set to the files where autodeps might live.
UserCommand String -- ^ The command the user thinks about, before any munging. Defaults to the actual command.
FSAOptions String -- ^ Options to @fsatrace@, a list of strings with characters such as @\"r\"@ (reads) @\"w\"@ (writes). Defaults to @\"rwmdqt\"@ if the output of @fsatrace@ is required.
NoProcessGroup -- ^ Don't run the process in its own group. Required when running @docker@. Will mean that process timeouts and asyncronous exceptions may not properly clean up child processes.

EchoCommand Bool -- ^ Print each command to stdout before it is executed. We call this echoing because it gives the appearance that you are typing the lines yourself.

-v, --verbose
  show all command lines while building, as if all recipes had EchoCommand True

‘-s’ ‘--quiet’
    Quiet operation; do not print the commands as they are executed, as if all recipes had EchoCommand False.

IgnoreExitStatus Bool -- ^ when false: If there is an error (the exit status is nonzero), throw an error and stop executing the thunk.when True: print exit status if non-zero and continue execution.

‘-i’ ‘--ignore-errors’
    Ignore all errors in commands, as if all recipes had IgnoreExitStatus True.

--skip-commands, RunCommands :: Bool
  Default to 'True'. Set to 'False' to skip all command line actions (treat each command as an operation that does nothing, produces no output on stdout/stderr, and returns a 0 exit code). Useful for profiling the non-command portion of the build system.

Querying the build graph
------------------------

The build graph defines how to tell whether a thunk needs recompilation, and the entry point to update the thunk. But running the thunk is not always what you want; sometimes you only want to know what would be run.

‘-n’
‘--dry-run’

    “No-exec”. Print the thunks that would normally execute to make the targets up to date, but don't actually execute them or modify the filesystem. This is implemented by processing the output from the simulation; certain to execute, likely to execute, certain to substitute, likely to execute but possible to substitute, likely to be skipped. This flag is useful for finding out which thunks Cot thinks are necessary without actually doing them.

‘-q’
‘--question’

    “Question mode”. Silently check whether the targets are up to date. Do not run any recipes, or print anything; just return an exit status code that is zero if the specified targets are already up to date, one if any updating is required, or two if an error is encountered. This is implemented by running as normal but aborting if a thunk is actually executed.

Forcing/avoiding recompilation
------------------------------

if your build system is broken then you can't fix it with the ``touch`` utility. so a command ``--touch`` that forces files to be invalid seems necessary, although it wouldn't be needed normally.

‘-t’
‘--touch’

    Touch files - mark the build as up to date without actually running it, pretending that the build was done but no output files changed, in order to fool future invocations of make. make walks through the build graph and modifies each initial filesystem input recorded in a thunk record to match the state from the filesystem. The name of the modified thunk is also printed, ``touch $thunk``, unless ‘-s’ or .SILENT is used. Note that intermediate or output files are not recorded, so they will still appear as damaged if they are modified and touch is run.

Sometimes you may have changed a source file but you do not want to recompile all the files that depend on it. For example, suppose you add a macro or a declaration to a header file that many other files depend on. Being conservative, make assumes that any change in the header file requires recompilation of all dependent files, but you know that they do not need to be recompiled and you would rather not waste the time waiting for them to compile.

If you anticipate the problem before changing the header file, you can use the ‘-t’ flag. This flag tells make not to run the recipes in the rules, but rather to mark the target up to date by changing its last-modification date. You would follow this procedure:

    Use the command ‘make’ to recompile the source files that really need recompilation, ensuring that the object files are up-to-date before you begin.
    Make the changes in the header files.
    Use the command ‘make -t’ to mark all the object files as up to date. The next time you run make, the changes in the header files will not cause any recompilation.

If you have already changed the header file at a time when some files do need recompilation, it is too late to do this. Instead, you can use the ‘-o file’ flag, which marks a specified file as “old” (see Summary of Options). This means that the file itself will not be remade, and nothing else will be remade on its account. Follow this procedure:

    Recompile the source files that need compilation for reasons independent of the particular header file, with ‘make -o headerfile’. If several header files are involved, use a separate ‘-o’ option for each header file.
    Touch all the object files with ‘make -t’.

"B" ["rebuild"] (optArg "PATTERN" $ \x s -> s{shakeRebuild=shakeRebuild s ++ [(RebuildNow, fromMaybe "**" x)]}) "If required, these files will rebuild even if nothing has changed."
""  ["no-rebuild"] (optArg "PATTERN" $ \x s -> s{shakeRebuild=shakeRebuild s ++ [(RebuildNormal, fromMaybe "**" x)]}) "If required, these files will rebuild only if things have changed (default)."
""  ["skip"] (optArg "PATTERN" $ \x s -> s{shakeRebuild=shakeRebuild s ++ [(RebuildLater, fromMaybe "**" x)]}) "Don't rebuild matching files this run."
,yes $ Option ""  ["skip-forever"] (OptArg (\x -> Right ([], \s -> s{shakeRebuild=shakeRebuild s ++ [(RebuildNever, fromMaybe "**" x)]})) "PATTERN") "Don't rebuild matching files until they change."

The make tool has a number of features to force rebuilds or skip rebuilds, all fundamentally modelled on file modification times forming an order, which is quite a different model to Shake.

-B / --always-make considers all targets out-of-date and rebuilds everything. The Shake equivalent is --rebuild.
-o FILE / --old-file=FILE / --assume-old=FILE does not remake the file FILE even if it is older than its prerequisites. The Shake equivalent is --skip=FILE.
-t / --touch touches files (marks them up to date without really changing them) instead of building them. The closest equivalent in Shake is --skip, but that only applies to this run. A hypothetical RebuildNever flag would more accurately model this flag.
-W FILE / --what-if=FILE / --new-file=FILE / --assume-new=FILE pretends that the target file has just been modified. Shake doesn't really have an equivalent, as --rebuild applies to the rules to rebuild, whereas in Make this applies to the things that depend on it. In addition, Make often uses this flag in conjunction with dry-run, which Shake doesn't yet have.


Rebuild :: [(Rebuild, FilePattern)]
 ^ What to rebuild

RebuildNormal is the default setting, rebuild a rule if its dependencies have changed.
RebuildNow forces a rule to rebuild even if its dependencies haven't changed. If the rule changes, then that will in turn cause anything depending on that rule to rebuild too. Useful to undo the results of 'RebuildNever'.
RebuildLater causes a rule not to rebuild this run even if its dependencies have changed. Note that in future runs, if the RebuildLater is not set, the rule may rebuild.
RebuildNever permanently marks a file as up-to-date. This assumption is unsafe, and may lead to incorrect build results in this run, and in future runs. Assume and record that these files are clean and do not require rebuilding, provided the file has been built before. Useful if you have modified a file in some inconsequential way, such as only the comments or whitespace, and wish to avoid a rebuild.

 --config=mode

    Control how the Configure call should use or generate the results of configuration tests. modeshould be specified from among the following choices:

    auto

        scons will use its normal dependency mechanisms to decide if a test must be rebuilt or not. This saves time by not running the same configuration tests every time you invoke scons, but will overlook changes in system header files or external commands (such as compilers) if you don't specify those dependecies explicitly. This is the default behavior.

    force

        If this option is specified, all configuration tests will be re-run regardless of whether the cached results are out of date. This can be used to explicitly force the configuration tests to be updated in response to an otherwise unconfigured change in a system header file or compiler.

    cache

        If this option is specified, no configuration tests will be rerun and all results will be taken from cache. scons will report an error if --config=cache is specified and a necessary test does not have any results in the cache.

‘-B’
‘--always-make’

    Consider all targets out-of-date. GNU make proceeds to consider targets and their prerequisites using the normal algorithms; however, all targets so considered are always remade regardless of the status of their prerequisites. To avoid infinite recursion, if MAKE_RESTARTS (see Other Special Variables) is set to a number greater than 0 this option is disabled when considering whether to remake makefiles (see How Makefiles Are Remade).

‘-W file’
‘--what-if=file’
‘--assume-new=file’
‘--new-file=file’

    Pretend that the target file has just been modified. When used with the dry run flag, this shows you what would happen if you were to modify that file. Without dry run, it is almost the same as running a touch command on the given file before running make, except that the modification time is changed only in the imagination of make. See Instead of Executing Recipes.

    “What if”. Each ‘-W’ flag is followed by a file name. The given files’ modification times are recorded by make as being the present time, although the actual modification times remain the same. You can use the ‘-W’ flag in conjunction with the ‘-n’ flag to see what would happen if you were to modify specific files.

The ‘-W’ flag provides two features:

    If you also use the ‘-n’ or ‘-q’ flag, you can see what make would do if you were to modify some files.
    Without the ‘-n’ or ‘-q’ flag, when make is actually executing recipes, the ‘-W’ flag can direct make to act as if some files had been modified, without actually running the recipes for those files.

‘-o file’
‘--old-file=file’
‘--assume-old=file’

    Do not remake the file file even if it is older than its prerequisites, and do not remake anything on account of changes in file. Essentially the file is treated as very old and its rules are ignored. See Avoiding Recompilation of Some Files.

Error handling
--------------

"k" ["keep-going"] (noArg $ \s -> s{shakeStaunch=True}) "Keep going when some targets can't be made."
"S" ["no-keep-going","stop"] (noArg $ \s -> s{shakeStaunch=False}) "Turns off -k."
shake staunch mode: if an error is encountered during the middle of a build, unless --keep-going is specified we want to stop the build. we can stop all the threads immediately by sending cancel commands, or we can wait until each command finishes to interrupt.

When an error happens that propagates out of the thunk, it implies that the current thunk cannot be correctly remade, and neither can any other thunk that is chronologically after. No further thunks will be executed after the thunk, since the preconditions have not been achieved.

‘-k’
‘--keep-going’
-k N

    keep going until N jobs fail (0 means infinity) [default=1]
    Continue as much as possible after an error. While the target that failed, and those that depend on it, cannot be remade, the other prerequisites of these targets can be processed all the same. See Testing the Compilation of a Program.

‘-S’
‘--no-keep-going’
‘--stop’

    Cancel the effect of the ‘-k’ option. This is never necessary except in a recursive make where ‘-k’ might be inherited from the top-level make via MAKEFLAGS (see Recursive Use of make) or if you set ‘-k’ in MAKEFLAGS in your environment.

Staunch :: Bool
 ^ Defaults to 'False'. Operate in staunch mode, where building continues even after errors,
   similar to @make --keep-going@.

Normally make gives up immediately in this circumstance, returning a nonzero status. However, if the ‘-k’ or ‘--keep-going’ flag is specified, make continues to consider the other prerequisites of the pending targets, remaking them if necessary, before it gives up and returns nonzero status.

Normally, when an error happens in executing a shell command, make gives up immediately, returning a nonzero status. No further recipes are executed for any target. The error implies that the goal cannot be correctly remade, and make reports this as soon as it knows.

When you are compiling a program that you have just changed, this is not what you want. Instead, you would rather that make try compiling every file that can be tried, to show you as many compilation errors as possible.

On these occasions, you should use the ‘-k’ or ‘--keep-going’ flag. This tells make to continue to consider the other prerequisites of the pending targets, remaking them if necessary, before it gives up and returns nonzero status. For example, after an error in compiling one object file, ‘make -k’ will continue compiling other object files even though it already knows that linking them will be impossible. In addition to continuing after failed shell commands, ‘make -k’ will continue as much as possible after discovering that it does not know how to make a target or prerequisite file. This will always cause an error message, but without ‘-k’, it is a fatal error (see Summary of Options).

The usual behavior of make assumes that your purpose is to get the goals up to date; once make learns that this is impossible, it might as well report the failure immediately. The ‘-k’ flag says that the real purpose is to test as much as possible of the changes made in the program, perhaps to find several independent problems so that you can correct them all before the next attempt to compile. This is why Emacs’ M-x compile command passes the ‘-k’ flag by default.

For example, after an error in compiling one object file, ‘make -k’ will continue compiling other object files even though it already knows that linking them will be impossible.
The usual behavior assumes that your purpose is to get the specified targets up to date; once make learns that this is impossible, it might as well report the failure immediately. The ‘-k’ option says that the real purpose is to test as many of the changes made in the program as possible, perhaps to find several independent problems so that you can correct them all before the next attempt to compile. This is why Emacs’ compile command passes the ‘-k’ flag by default.

Usually when a recipe line fails, if it has changed the target file at all, the file is corrupted and cannot be used—or at least it is not completely updated. Yet the file’s time stamp says that it is now up to date, so the next time make runs, it will not try to update that file. The situation is just the same as when the shell is killed by a signal; see Interrupts. So generally the right thing to do is to delete the target file if the recipe fails after beginning to change the file. make will do this if .DELETE_ON_ERROR appears as a target. This is almost always what you want make to do, but it is not historical practice; so for compatibility, you must explicitly request it.
