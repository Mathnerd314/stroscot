Build system
############

The name Cot is unimaginative but suffices for now.

Justification
=============

The compiler needs a build system integrated so that intermediate results such as checked/optimized functions can be stored efficiently and rebuilt only when needed, package dependencies and generated files can be discovered and created while building, and autoconf results can be reused so it only has to be run once per system. A powerful build system is the only way to handle complex builds.

Also we need a way to do multi-language dependency management and ensure that there is a single version of each significant piece of code.

Scale
=====

As of 2016, the Google repo has 1 billion files, of which 9 million are code. The goal is to take at most 3 seconds to build this repo, on a single beefy desktop computer connected to a cloud build farm. Also, it should be able to build a large executable like Firefox (21M lines of code, 10,000-ish files) on an isolated system, in around the same time.

There are several types of build. If we trust our build system, we never need to do a from-scratch build. But it is good to trust no one and to do from-scratch builds occasionally. Those might take seconds, hours, days, or weeks depending on the scale - it doesn't really matter because they're rare. They just have to be possible.

No-op builds should be "instantaneous" (<100 milliseconds per `SO <https://ux.stackexchange.com/questions/16253/defining-instantaneous-as-part-of-usability-acceptance-criteria>`__), this is easy enough to do with a file change watcher. For incremental builds that do a small change to 1 file, let's say it rebuilds from 1 - 1000 files depending on where it is in the dependency graph. Presumably you've architected the system so "change one file, rebuild a million" is rare. Taking average numbers (500 files, 2100 lines/file), achieving 10 second rebuild times (max acceptable UI lag per `Nielsen <https://www.nngroup.com/articles/response-times-3-important-limits/>`__`) requires 105k/s. Practically I think go's 100k/s is probably adequate.


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
    IKey -> "Rule engine"
    "Rule engine" -> OKey
    {
    rank=same
    OKey [label="Output keystate"]
    OKey2 [label="Updated keystate"]
    }

    "Rule engine" -> "Trace database"
    "Trace database" -> "Incremental rule engine" [dir=both]

    IKey -> "Changelist"
    IKey2 -> "Changelist"

    "Changelist" -> "Incremental rule engine"
    "Incremental rule engine" -> OKey2
  }

Broadly, a build system is a set of rules intended to be run by an execution engine (rule engine) that has logic for incremental computing. We use the term "keystate" to refer to an arbitrary dataset consisting of keys and values; it may be files, a Web API, a database, or some observations of the system time or ``/dev/random``. A clean build consists of the rule engine using the input keystate to execute to completion to produce the output keystate.

Logically, there is a clear separation between inputs and outputs. Inputs are consumed by the build system (e.g. system header files or ``/dev/random``), while outputs are produced by the build system (e.g. an executable, a private key). A source code formatter tidying up file during the build consists of an input (the original file) and an output (the tidied file) indexed by the same filename. In general the input keystate may not be accessible after the build. Note that a file that is overwritten before any data is read from it is not an input; but the existence of the file might be an input.

For incremental builds (builds that aren't clean), we also have reference to the previous runs of the engine, in the form of traces produced by the rule engine. In the simplest case it is only the trace of the previous run that is available, but a caching build system with constructive traces ("build farm") can maintain many sets of traces in parallel and interpolate between them, a trace database. And the incremental build itself produces a trace.

To correctly build software, we assume for each task that the recorded read operations uniquely determine the execution behavior of the task, specifically its write and synchronization operations.

Determining the input keystate for an incremental build is a bit tricky, because the output files from a clean build may become input files for the incremental build. Generally we don't want to mix in artifacts produced from previous builds. Hence we define the inputs of the incremental build as the inputs of the clean build plus any other files so long as those files are not the outputs of the clean build. But we may include some output files as well, as exceptions.

Another issue is "time travel", a task reading a file from the previous build that hasn't yet been generated in this build. Cot supports a build cache, so this can be prevented by deleting all the build files before the build and then copying them back as needed. But this is more easily detected after-the-fact, particularly in the case of parallel builds.

Tasks
=====

The design is based on iThreads :cite:`bhatotiaIThreadsThreadingLibrary2015`, which out of a half dozen incremental computation papers seemed the most appealing. The approach supports arbitrary multithreaded shared-memory programs, and hence imposes no requirements on the overall flow of the build.

They call the units of computation "thunks", but this term doesn't have a good intuition. A thunk usually is a delayed value, but here the unit does not return a value and simply modifies the shared keystate. So I'm using the term "task" in the sense of a task queue.

It is relatively simple to add new synchronization operations; the only constraint is that they are executed on every build and rebuild, so should not be expensive.

The ideal memory model is causal consistency: a task A should see only the write of a task B only if the synchronization operation that started A ensured that B was finished first. The real model is that the keystate uses a global shared memory for performance. Hence the build can have data races where different execution orders produce different results.

If an output is modified or deleted, the clean build semantics dictates that it will be regenerated from the inputs. But a lot of the time we don't care about most of the outputs (intermediate files) so Cot includes damage handling logic to compute the minimal rebuild for the desired outputs.

Task names
===========

Task naming adds some complexity to the implementation of a build system, as the task names also affect the way computations can be re-used. iThreads uses a simple "thread # task #" scheme, which assumes a fixed number of long-running threads and invalidates all of the tasks in a thread after a modified task. A scheme similar to ``make`` uses filenames; for each file f there are two tasks "run f" and "exec f". The "run f" just does ``Sequence [subtargets,["exec f"]]`` while "exec f" runs the commands that generate f. But with fine-grained dependency tracking we can track each command separately - we could use task names like "exec f step #" but this leads to invalidating later tasks. Using names like "exec f step cmd" requires a lot of boilerplate names to be written out. The ideal solution is probably some form of structural hashing.

Also, in a dynamic build, a direct file action map like this is not always available, and so the naming scheme must be relaxed to allow dependencies on things that aren't files. For example, we may have one command that generates two files; so long as we use a consistent task name for this command there is no issue. For another example, we may have include headers that are picked up in a search path directory listing. To deal with this directly, we would need to introduce build logic into the search mechanism and run dependencies when seeing ``#include``. But a phase separation handles it fine with minimal changes - we generate the files first and then call the compiler, filling in the build dependencies from an output list of used headers. In this case we would need tasks for each phase.


Model
=====

To reason about the behavior we need a pencil-and-paper model of how it works. First we have task IDs (``tid`` s); these come from the program and are quoted strings "abc". For key names we use unquoted strings xyz and for key values integers 123; these are only compared for equality (often they are modification times). Then for the traces we use a tabular format to record the reads, writes, and synchronization operations. We might have databases from multiple runs available, so there is also a "machine" column, but this is the same for all rows in a single trace so it is omitted here. An example database based on the example in :cite:`shalBuildSystemRules2009` might be

.. raw:: html

  <style>
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
        node [shape="circle",fontsize=20]
        "main.c", "main.o", "prog", "parse.o", "parse.h", "parse.c", "parse.y" [shape="rect"]

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

Circular nodes represent tasks while rectangular nodes are keys (files). Black lines are reads. Blue lines are writes. Dotted gray lines are sequenced to execute before solid gray lines. Overall, the graph structure is very similar to Pluto's two-level graph, but the control structure is more complex - Pluto simply has build-require, while Cot has various synchronization operations.

Then during an incremental run we start with a list of changed keys and their values; this is allowed to contain unmodified keys, so generating this list may be as simple as calculating the state of all keys and saying they all might be modified, or it may be a more precise list from a filesystem watcher or similar API. The keys can also include volatile information such as FTP server listings or stdin.

.. csv-table::
  :header: key,value
  :quote: ^
  :widths: auto

  parse.y,1
  main.c,5

Here main.c's modification time has been updated. We start from the top and load "run prog"; there are no changed inputs (or indeed any inputs), so we skip execution of the task, perform the record write operations to the key state, and execute the synchronization operation, which loads "run main". "run main" loads "yacc" which has not changed, so control returns to "run main" and "cc main" is loaded. "cc main"'s inputs have changed, so we run it, producing an updated main.o. Meanwhile "run parse" and "cc parse" have been loaded with no changes. Control returns to "run prog" and "ld" is executed as its inputs have changed, building the final executable "prog".

Task state
===========

Task state is a bit tricky to define precisely. So let's work it out.

First we define execution state. A task is enabled once a synchronization operation requests to execute the task. A task is resolved once it is enabled and its synchronization operation has begun execution. So a task starts out disabled, becomes enabled, and then is resolved.

If a task is never enabled, then in a clean build the task would not be executed at all. There are two possibilities:

* unused: The task is not referenced by any trace or by the current build. Example: almost any arbitrary task id
* stale: The task is referenced by some trace but is not enabled anytime in the current build. Example: control flow change

Otherwise, the task is enabled. All enabled tasks will eventually be resolved - even if the task errors this is considered resolution. But all resolved tasks have local traces and can be considered in the resolved state. Similarly if it is decided to run the task then it's just in the running state.

* resolved: The task has been run, substituted, etc. and its synchronization operation was set up. Can be clean or damaged.
* running: The task is currently in progress

The interesting task state is if the task hasn't started running yet and we're thinking about running it. If a task is enabled, then we can consider the available traces and compare them with the keystate at the point the task is enabled. There is one trivial possibility:

* new: The task is not referenced by any trace but has been enabled in the current build. Examples: control flow change, clean build

When we have at least one trace, things get more interesting. A trace is valid if all of its recorded reads match the state of the build. The state on disk also becomes relevant.

* dirty: There are traces but no valid trace. Example: input change
* clean: There is a valid trace where all recorded writes match the state on disk. Example: A task is always clean immediately after it is executed, since running a task records its trace.
* damaged: There is at least one valid trace but no valid trace has its recorded writes matching the state on disk. Examples: shallow build, external modification, overwritten output

After resolving the task, it can only be clean or damaged; the clean state may have been achieved by substitution, reuse, or rebuilding, while the damaged state can only be from a damaged task passing the no-future-use check.

In a cloud build setting we have one more state to handle constructive traces. A constructive trace stores the full value for each key and allows fetching the output files without running the build.

* substitutable: There is a valid constructive trace.

A substitutable task can be clean or damaged but not dirty. So in total we have 8 states: unused, stale, new, dirty, clean-nonsubstitutable, clean-substitutable, damaged-nonsubstitutable, and damaged-substitutable. It's a lot, but Cot deals with a lot of functionality.

Simulation
==========

It's possible for a task to be handled in several ways: leave damaged/clean, rebuild, or substitute with a cloud version. These also have different costs: leaving things alone is free, substituting costs some amount of network bandwidth time / decompression, while rebuilding costs CPU time that can be estimated from other builds. But to figure out the least-cost action overall we need a global view of the build. Damaged tasks can only be left alone if they are not needed during the rest of the build, i.e. no rebuilding task reads the damaged data. Substitutions from different sources may be incompatible (e.g. GHC used to produce `randomized symbols names <https://gitlab.haskell.org/ghc/ghc/-/issues/4012>`__), so picking the version influences the substitutability of other tasks.

The problem is NP-hard since we can encode 3-SAT in the substitution versions :cite:`coxVersionSAT2016`. Since it's that hard, we use a SAT solver. In particular we encode it as an instance of partial weighted MaxSAT. First we have a lot of hard constraints:

* each task can be left alone, substituted, or built, and we can only do one: ``t_leave + t_rebuild + t_v1 + ... + t_vn = 1`` (this is a pseudo-Boolean constraint that be easily encoded)
* For substitution, compatibility on the read/write values, ``t_vj -> (s_vx or s_vy or ...)``, where t reads a value that s writes and vx,vy, etc. are the versions of s that are compatible with version vj of t.
* For rebuilding, a conservative assumption that all outputs will be changed, ``s_rebuild -> t_rebuild`` where t reads from what s writes, and a requirement that rebuilds not use damaged data, ``t_rebuild -> not s_leave``, where s is damaged and t reads from s.

Then we have soft constraints for each variable weighted with the cost of using that option.

To generate these constraints, Cot walks through the build graph and maintains a multi-valued state. So it would look like ``Key i -> [Value 1 S_1, Value 1 S_2, Value 2 S_3, Damaged S_leave]``. Then for each task (visited in normal traversal order) Cot generates the constraints for each possibility. Then Cot updates the possible values for the keys it writes.

To deal with these constraints we need a MaxSAT solver - we can write a custom one or interface with an existing one. Using an off-the-shelf solver might save some effort, but there is significant overhead in serializing the constraints to an external solver, and this overhead can be avoided by using a native solver. The native solver will probably be naive and not have the finely tuned optimizations or heuristics of the off-the-shelf solvers, but most package version problems are very simple to solve. It'll be easier to build the project with a native solver because all of the code will be in the same language (Haskell or Stroscot). In Cox's list of package managers (at the end of :cite:`coxVersionSAT2016`), the split is 9-5 in favor of a native solver (although 3 of the native-solver package managers allow using an external solver with an option, so more like 9-8). Overall it seems writing a native solver is the best course of action. But we don't have to start from scratch as there is a Haskell MaxSAT solver in toysolver on Hackage.

Wanted files
------------

When using Cot as a package manager rather than a build system, we have lots of produced files that aren't used by anything. Since Cot doesn't see any users of the files it'll leave them as damaged (unmaterialized) and not download them. So at the end of the build process we'd run special tasks that simply read in a bunch of files, to ensure that the files are up-to-date and available for use. These tasks are always out of date, which can be though of as having a special wanted key that always compares unequal. In the end these special tasks are actually the packages.

We could also add functionality to force realizing specific damaged tasks.

Restarting
----------

The constraint model is only an approximation of the truth, in particular it doesn't cover a newly-executed task that adds a dependency on damaged data. The restarting strategy restarts build execution from the damaged task on detection of a read, which allows the build to continue if there is an unexpected dependency on damaged data. It requires traversal of the build graph to reconstruct the keystate at the point of re-execution, and all the work done after the point of re-execution is thrown away, so its efficiency isn't optimal. In particular it is possible to re-execute a unit several times, in the case where we execute a unit B, then go back and re-execute a unit A due to damage, then have to execute B another time due to A changing C changing input to B.

Graph pruning
=============

Pruning the build graph as pioneered by Tup can result in a big speedup, only having to load/inspect the part of the build graph that's necessary. But it requires some auxiliary data structures and careful record-keeping in order to look up the pieces efficiently.

We start with a change list, i.e. things that might have changed since our last build. The prototypical example is a list of changed files from a file-watching daemon. The alternative is scanning all the files for changes on startup. This can take several minutes with a hashing algorithm or a few seconds with modtimes.

First we process the change list into a list of possibly-changed keys. There are many various options (digest, modtime, etc.), so we need a hash table that maps key writes to all the tasks with key reads, really a filename->(set of task) table.

So in our build example, we would go from "main.c" to "cc main". Next we want load the other tasks "run main", "run prog", "ld". The first two are the ancestors of the task; we have to load the parent to see its synchronization operation and thus the order of execution. But we don't have to load any children of the parents.  So we just need a task->(task parents) map to find all the parents.

We also have to load "ld"; this is done by looking up the writes of "cc main" in the filename->task table. We need to load tasks that read from the writes during execution, in case they are different from the recorded writes.

Note that we'll always load the initial task, because we load the chain of parents. So after everything is loaded, execution can start from the initial task as normal, no need for a topological sort like in Tup. The difference is that we may have unloaded tasks as children; we do not want to execute these. But to keep the keystate consistent we need to be able to modify the keystate as though they were executed. In particular for each task we need the list of all the writes performed by the task and its children. But the task itself already stores the writes in its TaskRecord; so computing the total writes is a matter of combination, ``Total = Task // Union(Children)``, where ``//`` is record update. These write lists can be precomputed during the initial run. Storing them efficiently with fast access is a little tricky since there is a lot of copying in the lists. For now I'll store the full write list for each task, compressed, but there is probably a persistent data structure (`tree <https://en.wikipedia.org/wiki/Self-balancing_binary_search_tree>`__\ ?) that can efficiently re-use the data from other tasks while maintaining performance. At the other extreme we can just regenerate all the write lists by walking the task records, so these write lists can be cached and expired using LRU or something.
We also need to store the list of acquire/release lock operations, but most programs don't use locks so this will be small.

The write lists can also be used as an incomplete check for data races; if after executing a task A, A has read a key from the global/shared keystate with a value different from the local keystate passed into the task (state passed into the parent task P // modifications of P // modifications synced in from synchronization operation of P), then a task not in the execution history of A must have modified the key - since this execution could have been delayed by the scheduler, it is a read-write data race. Similarly in the union of the children, if there are differing values among the children then there is a write-write data race.

Anyway, the recorded state also records if the key is damaged and the task that regenerates it. So we can use this during our damage simulation to load in damaged tasks when referenced and re-run them if necessary.

Cleaning
========

When we re-execute a task, it is a good idea to restore the state of the outputs of the task to their original state (typically deleting them). Also at the end of the run we should garbage collect any unused tasks from the old run by deleting their outputs. Also in (hopefully rare) cases we want to delete all the outputs regardless of status.

-c, --clean, --remove

    Clean up by removing the selected targets, well as any files or directories associated with a selected target through calls to the Clean function. Will not remove any targets which are marked for preservation through calls to the NoClean function.

--clean-old

    clean built files that are no longer produced by the current build. A bad idea if there are multiple configurations that build different subsets. Basically we load all the tasks, then anything not loaded is not needed and its files etc. can be deleted.

Exceptions
==========

Shake tries to be exception-safe, but it's not clear what to do with exceptions besides passing them along. The top-level build function can throw exceptions, or it can catch them, printing them and exiting with an error code.

Trace database
==============

A robust build system design fundamentally depends on keeping a database of build traces. In particular to rebuild a command like ``cat src/*`` we must store the file list so as to detect deleted/added files.

For now the database is just a simple SQLite database with a few indexes, as having a working system is 90% of the work. But there are likely ways to speed it up (the other 90% of work).

We could store this in a file, but an append-only journal is crash-tolerant and less HD-intensive. Since file paths have lots of redundant components, some lightweight streaming compression like lz4 is appropriate.

We record all of the process/thread semantics, with fork, locks, wait/signal, etc. as well as its I/O. The tasks's version number / digest of its source code is also relevant. Reading the journal back, we end up with a list of interleaved thread traces.

Requesting execution of other tasks can be done sequentially or in parallel.

There are 3 main operations that show up in a task's trace:

* writing a key
* reading a key
* requesting execution of other tasks

To correctly build software, we assume that the task is deterministic besides the operations recorded in its trace - so the task can be skipped if all of its inputs and generated files are the same.

A key definition consists of:
* a set of key names, where each name is a sequence of bytes
* for the write operation:

  * a recorder, which saves the write to disk during a clean build
  * a replayer, which uses the stored trace to either determine that the

In-memory
---------

In-memory keys are the simplest to handle, because they're small and we can simply store the whole value, and also because we don't have to worry about external modification. We record a write in our journal as "write key xyz = ..." and a read as "read key xyz = ...". Then the trace is invalid if we read something different from what was written, or if the key was never written.

If the key contents are large, we can intern it - journal an association "#5 = x", then writes as "write key xyz is interned to #5 = ...", and reads as "read key xyz from intern #5". We can't use the key itself as #n because there might be multiple writes to the key.

The simplest example of an in-memory key is the command line arguments; we can store the full initial command line, and then have a task that parses the command line and writes various option keys. Another example is versioning keys. The initial task writes a key for each task with the compiled-in version, ``write (Version abc) v2.3``. Then each task reads its version and this read is stored in the task record, causing rebuilds when the version is changed.

--ignore-rule-versions
  Ignore versions in the build rules.

Files
-----

Files are a little trickier because storing the whole contents of the file in the journal is infeasible. Instead we journal a proxy of the contents, stored in-memory. So writes look like "write file f with proxy p" and reads are "read file f with proxy p". We assume that there aren't any untracked writes during the build so the reads can be recorded using the in-memory value of p calculated from the writes.

trivial proxy
  Sometimes we want to ignore the file contents and always/never do an action. In such a case we can use a trivial proxy. There are two types, "always rebuild" and "never rebuild". In the never case, a task's rebuild can still be triggered by a different file.

dirty bit
   The idea of a dirty bit is to have one piece of information per key, saying whether the key is dirty or clean. In the initial state all keys are clean. If a task executes, all its writes set the keys to dirty. A task that reads a dirty key must also execute. But if all read keys are clean, the task does not need to be rerun.

version number/custom detector
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

* digest-only: Files change when digest changes. Use if modification times on your file system are missing or don't update on changes.
* modtime-only: Files change when modtime changes. Use if your timestamps change mostly in sync with the file content
* modtime-then-digest: Files change when modtime and digest change. Use if you could use modtimes but want to avoid spurious rebuilds. In particular git touches a lot of files when switching branches, vim copies over the file so its inode changes frequently, and scripts/you can write identical files.
* modtime-then-digest-for-inputs: modtime-only for generated files and modtime-then-digest for inputs. It skips digests for generated files as they're large and change with almost every rebuild. Generated file modtimes can be kept constant by writing to a temporary file and only replacing the output if it's different.
* watcher-only, if your watcher runs continuously or if you delete all files after every run
* modtime-then-watcher: if your watcher's change journal is incomplete, do a modtime scan on startup.
* modtime-then-watcher-then-digest, to get the fastest file tracking and fewest rebuilds

Symlinks
~~~~~~~~

-L, --check-symlink-times

    On systems that support symbolic links, this option causes make to consider the timestamps on any symbolic links in addition to the timestamp on the file referenced by those links. When this option is provided, the most recent timestamp among the file and the symbolic links is taken as the modification time for this target file.

io_uring
~~~~~~~~

It's a little overkill, but the io_uring interface on Linux allows batching up calls asynchronously, which can `speed up stat() <https://twitter.com/axboe/status/1205991776474955777>`__ and thus modtime reading . For hashing parallelism is likely counterproductive, as xxHash is I/O bound and parallelism turns sequential reads into random reads.

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

* HTTP downloads: we can use wget, curl, aria2, or a custom library. The `caching headers <https://developer.mozilla.org/en-US/docs/Web/HTTP/Caching>`__ are important for re-using old downloads.
* FTP: this can be treated similarly to the filesystem
* Git, Bittorrent, IPFS: these are content-addressed stores so keeping track of the hash is sufficient

A more complex example is deploying a container to AWS. The inputs are: all the configuration details for the host, the container image itself, and secret credential information. The output is a running instance or else a long log file / error message. But the running instance cannot be checksummed, so we must use some proxy criterion - the easiest is to redeploy if any inputs have changed, but we could also use a script to interrogate the running instance over the network.

If there are multiple containers that depend on each other, we have to encode the restarting behavior somehow. The easiest is probably to write a single script that takes all the configuration and starts up the containers in order, but this duplicates the build system task scheduling logic. So a script for each strongly-connected component.

Damage
------

Cot allows writing to a file more than once, e.g. training a neural net with iterative optimization. The behavior is that changed inputs always rerun all affected tasks, but changed outputs only rerun the tasks if the simulation predicts that the output is needed. If a build cache is not used then tasks that generate files needed for the build will rerun as well.

Options
=======

* ``-m, --metadata`` The directory used for storing metadata files. All metadata files will be named ``$files/$file-name``. If the 'shakeFiles' directory does not exist it will be created. If set to ``Nothing`` then no metadata files are read or written (clean build mode). Defaults to ``.cot``.
* ``--flush N`` How often to flush metadata files in seconds, or ``--never-flush`` to never flush explicitly. On abnormal termination the completion data that has not been flushed will be lost.

Cached build
------------

A build cache records the outputs of each task in a reproducible manner, i.e. the trace is constructive in the sense of :cite:`mokhovBuildSystemsCarte2020`. A build can be made reproducible by forcing every non-reproducible task to be loaded from the cache.

--cache-create PATH
  Whether to use and store outputs in a shared directory. If present, retrieve files from the cache and copy files to the cache, subject to other options. The cache path is stored in the metadata for further invocations.

--cache-disable, --cache-delete
  The disable option can be used to temporarily disable the cache without modifying the cache, while the delete option deletes it.

--cache-links PATHS
  For files matching listed path patterns, make files in the cache read-only to avoid inadvertently poisoning the shared cache. Use hard links or reflinks to replay tasks, instead of copying files.

--cache-readonly
  Use the cache, if enabled, to retrieve files, but do not not update the cache with any files actually built during this invocation.

--cache-populate
  When using CacheDir, populate a derived-file cache by copying any already-existing, up-to-date derived files to the cache, in addition to files built by this invocation. This is useful to populate a new cache with all the current derived files, or to add to the cache any derived files recently built with caching disabled via the --cache-disable option.

--cache-check
    Sanity check the shared cache files.

--cache-cloud URL
  HTTP server providing a (read-only) cache in the cloud.

Dune has the ability to cache built files for later retrieval. This
can greatly speedup subsequent builds when some dependencies are
rebuilt in different workspaces, switching branches or iterating on
code back and forth.


Configuration
=============

The cache is, for now, an opt-in feature. Add `(cache enabled)` to
your dune configuration file (default `~/.config/dune/config`) to
activate it. When turned on, built files will automatically be
promoted to the cache, and subsequent builds will automatically check
the cache for hits.

The cached files are stored inside you `XDG_CACHE_HOME` directory on
\*nix systems, and `"HOME\\Local Settings\\Cache"` on Windows.


Daemon
======

By default, most cache operations go through the dune cache daemon, a
separate process that dune instances connect to. This enables
promotions to happen asynchronously and not slow the build
process. The daemon is automatically started if needed when dune needs
accessing the cache, and lives on for further use.

Although the daemon concept is totally transparent, one can control it
via the `dune cache` subcommand.

Starting the daemon
-------------------

Use `dune cache start` to start the caching daemon if not running and
print its endpoint, or retrieve the endpoint of the currently running
daemon otherwise. A notable option is `--foreground` to not detach the
daemon, which can help inspecting its log output.

Stopping the daemon
-------------------

Use `dune cache stop` to stop the caching daemon. Although the daemon,
when idle, should consume zero resources, you may want to get rid of
the process. Also useful to restart the daemon with `--foreground`.


Filesystem implementation
=======================================

Hardlink mode
-------------

By default the cache works by creating hardlinks to built files inside
the cache directory when promoted, and in other build trees when
retrieved. This has the great advantage of having zero disk space
overhead for files still living in a build directory. This has two
main constraints:

* The cache root must be on the same partition as the build tree.
* Produced files will be stripped from write permissions, as they are
  shared between build trees. Note that modifying built files is bad
  practice in any case.

Copy mode
---------

If one specifies `(cache-duplication copy)` in the configuration file,
dune will copy files to and from the cache instead of using hardlinks.
This can be useful if the build cache is on a different partition.

On-disk size
============

The cache daemon will perform periodic trimming to limit the overhead.
Every 10 minutes, it will purge the least recently used files so the
cache overhead does not exceed 10G. This is configurable through the
`(cache-trim-period SECONDS)` and `(cache-trim-size BYTES)`
configuration entries. Note that this operation will only consider the
cache overhead, i.e. files not currently hard-linked in a build
directory, as removing files currently used would not free any disk
space.

On can run `dune cache trim --size=BYTES` to manually trigger trimming
in the cache daemon.

Reproducibility
===============

Reproducibility check
---------------------

While default mode of operation of the cache is to speedup build times
by not re-running some rules, it can also be used to check build
reproducibility. If `(cache-check-probability FLOAT)` or
`--cache-check-probability=FLOAT` is specified either respectively in
the configuration file or the command line, in case of a cache hit
dune will rerun the rule anyway with the given probability and compare
the resulting files against a potential cache hit. If the files
differ, the rule is not reproducible and a warning will be emitted.

Non-reproducible rules
----------------------

If you know that some rule is not reproducible (e.g. it generates a random signing key) and should be done on each new build, then you can mark it as such by depending on the AlwaysRebuild key. But think about whether you want to do it every build or if there is a configurable policy, e.g. refreshing a file from the internet can be done on a schedule.

Similarly, some files may not be redistributable (copyright license or similar), these are reproducible but the data cannot be stored in the cloud cache.

Daemon-less mode
================

While the cache daemon provides asynchronous promotions to speedup
builds and background trimming amongst other things, in some
situations direct access can be preferable. This can be the case when
running in an isolated environment like Docker or OPAM sandboxes,
where only one instance of dune will ever be running at a time, and
access to external cache is prohibited. Direct filesystem access can
be obtained by specifying `(cache-transport direct)` in the
configuration file or passing `--cache-transport=direct` on the
command line.

Remote Builds
-------------

A remote build consists of a local build setup forwarding task invocations to other machines. This allows multiple builds to be performed in parallel and to do multi-platform builds in a semi-transparent way.

cot ping-builders
  Test whether connecting to each remote instance works. To forward a build to a remote machine, it’s required that the remote machine is accessible via SSH and that it has Cot installed. If you get the error ``cot: command not found`` then you need to ensure that the PATH of non-interactive login shells contains Cot.

Each machine specification consists of the following elements, separated by spaces. Only the first element is required. To leave a field at its default, set it to -.

    The URI of the remote store in the format ssh://[username@]hostname, e.g. ssh://nix@mac or ssh://mac. For backward compatibility, ssh:// may be omitted. The hostname may be an alias defined in your ~/.ssh/config. It is possible to specify an SSH identity file as part of the remote store URI, e.g. ``ssh://mac?ssh-key=/home/alice/my-key``. Since builds should be non-interactive, the key should not have a passphrase. Alternatively, you can load identities ahead of time into ssh-agent or gpg-agent, as SSH will use its regular identities.

    The maximum number of builds to execute in parallel on the machine. Typically this should be equal to the number of CPU cores. For instance, the machine itchy in the example will execute up to 8 builds in parallel.

    The “speed factor”, indicating the relative speed of the machine. If there are multiple machines of the right type, Cot will prefer the fastest, taking load into account.

    A comma-separated list of supported features and platform identifiers, such as ``i686-linux,x86_64-linux,kvm``. Cot will only perform the derivation on a machine that has the specified features.

    A comma-separated list of mandatory features. A machine will only be used to build a derivation if all of the machine’s mandatory features appear in the derivation’s features attribute.

Remote builders can be configured on the command line with ``--builders`` or in general conf or in a separate configuration file included in builders via the syntax @file.

builders-use-cache

    If set to true, remote hosts will fetch as many build dependencies as possible from a build cache, instead of upload the files from the host. This can drastically reduce build times if the network connection between this computer and the remote build host is slow. Defaults to false.

To build only on remote builders and disable building on the local machine, you can use the option --max-jobs 0.

Debugging
---------

browse dependency graph in a web browser
show dependencies stored in the deps log
output graphviz dot file for targets

profiling information

list all commands required to rebuild given targets
list all rules
show inputs/outputs for a path
list targets by their rule or depth in the DAG
dump JSON compilation database to stdout

recompacts internal data structures
restats all outputs in the build log

--version
  Print the version number and exit.

--storage-log
  Write a message to ``storage.log`` whenever a storage event happens which may impact on the current stored progress. Examples include database version number changes, database compaction or corrupt files.

--no-build
  Load all the database files but stop before executing the initial task and don't build anything.

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
   Provided for compatibility with ``make`` and ``ninja`` (which have ugly file creation semantics).
NeedDirectory :: Bool
 ^ Defaults to ``False``. Is depending on a directory an error (default), or it is permitted with
   undefined results. Provided for compatibility with ``ninja``.
VersionIgnore :: Bool
 ^ Defaults to 'False'. Ignore any differences in 'shakeVersion'.

dupbuild={err,warn}  multiple build lines for one target
phonycycle={err,warn}  phony build statement references itself

--cache-show

    When using a derived-file cache and retrieving a file from it, show the command that would have been executed to build the file. Without this option, scons reports "Retrieved 'file' from cache.". This allows producing consistent output for build logs, regardless of whether a target file was rebuilt or retrieved from the cache.

--cache-debug=file

    Write debug information about derived-file caching to the specified file. If file is a hyphen (-), the debug information is printed to the standard output. The printed messages describe what signature-file names are being looked for in, retrieved from, or written to the derived-file cache specified by CacheDir.

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
 ^ Default to ``[]``. After the build system completes, write a list of all files which were /live/ in that run,
   i.e. those which Shake checked were valid or rebuilt. Produces best answers if nothing rebuilds.
Report :: [FilePath]
 ^ Defaults to ``[]``. Write a profiling report to a file, showing which rules rebuilt,
   why, and how much time they took. Useful for improving the speed of your build systems.
   If the file extension is ``.json`` it will write JSON data; if ``.js`` it will write Javascript;
   if ``.trace`` it will write trace events (load into ``about:\/\/tracing`` in Chrome);
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

    reserved-variable

        Warnings about attempts to set the reserved construction variable names $CHANGED_SOURCES, $CHANGED_TARGETS, $TARGET, $TARGETS, $SOURCE, $SOURCES, $UNCHANGED_SOURCES or $UNCHANGED_TARGETS. These warnings are disabled by default.

    stack-size

        Warnings about requests to set the stack size that could not be honored. These warnings are enabled by default.

    target_not_build

        Warnings about a build rule not building the expected targets. These warnings are disabled by default.

Parallel Execution
------------------

--random, --random=SEED, --no-random

    Build dependencies in a random order (the default) or a deterministic order. This is useful to prevent various scheduling slowdowns in the build, and can reduce contention in a build farm.

‘-j [jobs]’
‘--jobs[=jobs]’

  Specifies the capacity of the CPU resource, which limits the maximum number of tasks that can run simultaneously. If there is more than one ‘-j’ option, the last one is effective.  Defaults to ``1``.
  For many build systems, a number equal to or slightly less than the number of physical processors
  works well. Use ``auto`` to use the detected number of processors.

‘-l [load]’
‘--load-average[=load]’
‘--max-load[=load]’

    Specifies that no new recipes should be started if there are other recipes running and the load average is at least load (a floating-point number). With no argument, removes a previous load limit.

Cot can execute several recipes at once. This is implemented using a resource system; by default each task consumes one "thread" resource and there are as many thread resources as there are physical processors. But you can specify the number of threads consumed and also define other resources so in general a task runs with a multiset of resources.

Numerical priorities with random tie-breaking seems enough to implement things like "schedule this long job first" or "prioritize this set of tasks that's related to a modified file". Automatically determining these things when build times are noisy and dependencies change frequently seems hard, and the usual case is lots of cheap tasks where scheduling is easy, so it doesn't seem worthwhile to implement a more complicated scheduler.

GNU Make allows defining a load limit instead of a thread limit, basically "pause new executions if the load is above some number". The hard part is that the load average isn't instantaneous, so it needs to be mixed with the number of jobs started recently, and also the load can never exceed the number of cores, so load limits above a certain level are invalid. In practice it seems nobody uses the load limit. Builds generally run on unloaded systems and predicting the load by counting threads and resources is more accurate. The useful feature seems to be measuring the system load on startup and subtracting that number from the number of cores to get a lower maximum thread count.

When finishing a task it wakes up all the pending tasks, this is implemented with callbacks.

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
 ^ Defaults to ``[]``. A list of substrings that should be abbreviated in status messages, and their corresponding abbreviation.
   Commonly used to replace the long paths (e.g. ``.make\/i586-linux-gcc\/output``) with an abbreviation (e.g. ``$OUT``).
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

With touch, the name of each modified task is printed, ``touch $task``, unless ‘-s’ is used.

Command Options
---------------

CommandOptions :: [CmdOption]
 ^ Defaults to ``[]``. Additional options to be passed to all command invocations.

Cwd FilePath -- Change the current directory of the spawned process. By default uses the parent process's current directory. If multiple options are specified, each is interpreted relative to the previous one: ``[Cwd "/", Cwd "etc"]`` is equivalent to ``[Cwd "/etc"]``.

‘-C dir’ ‘--directory=dir’
  A global version of Cwd that runs at the beginning. You should never change the current directory of the parent process after the build starts as multiple tasks running at the same time share the current directory.

Env [(String,String)] -- ^ Replace the environment block in the spawned process. By default uses this processes environment.
AddEnv String String -- ^ Add an environment variable in the child process.
RemEnv String -- ^ Remove an environment variable from the child process.
AddPath [String] [String] -- ^ Add some items to the prefix and suffix of the ``$PATH`` variable.

Stdin String -- ^ Given as the ``stdin`` of the spawned process. By default the ``stdin`` is inherited.
StdinBS LBS.ByteString -- ^ Given as the ``stdin`` of the spawned process.
FileStdin FilePath -- ^ Take the ``stdin`` from a file.
InheritStdin -- ^ Cause the stdin from the parent to be inherited. Might also require NoProcessGroup on Linux. Ignored if you explicitly pass a stdin.

Two processes cannot both take input from the same device at the same time. To make sure that only one recipe tries to take input from the terminal at once, make will invalidate the standard input streams of all but one running recipe. If another recipe attempts to read from standard input it will usually incur a fatal error (a ‘Broken pipe’ signal).

It is unpredictable which recipe will have a valid standard input stream (which will come from the terminal, or wherever you redirect the standard input of make). The first recipe run will always get it first, and the first recipe started after that one finishes will get it next, and so on.

WithStdout Bool -- ^ Should I include the ``stdout`` in the exception if the command fails? Defaults to 'False'.
WithStderr Bool -- ^ Should I include the ``stderr`` in the exception if the command fails? Defaults to 'True'.
EchoStdout Bool -- ^ Should I echo the ``stdout``? Defaults to 'True' unless a 'Stdout' result is required or you use 'FileStdout'.
EchoStderr Bool -- ^ Should I echo the ``stderr``? Defaults to 'True' unless a 'Stderr' result is required or you use 'FileStderr'.
FileStdout FilePath -- ^ Should I put the ``stdout`` to a file.
FileStderr FilePath -- ^ Should I put the ``stderr`` to a file.

BinaryPipes -- ^ Treat the ``stdin``\/``stdout``\/``stderr`` messages as binary. By default 'String' results use text encoding and 'ByteString' results use binary encoding.
CloseFileHandles -- ^ Before starting the command in the child process, close all file handles except stdin, stdout, stderr in the child process. Uses ``close_fds`` from package process and comes with the same caveats, i.e. runtime is linear with the maximum number of open file handles (``RLIMIT_NOFILE``, see ``man 2 getrlimit`` on Linux).

-- | Collect the ``stdout`` of the process.
--   If used, the ``stdout`` will not be echoed to the terminal, unless you include 'EchoStdout'.
--   The value type may be either 'String', or either lazy or strict 'ByteString'.
--
--   Note that most programs end their output with a trailing newline, so calling
--   ``ghc --numeric-version`` will result in 'Stdout' of ``\"6.8.3\\n\"``. If you want to automatically
--   trim the resulting string, see 'StdoutTrim'.
newtype Stdout a = Stdout {fromStdout :: a}

-- | Like 'Stdout' but remove all leading and trailing whitespaces.
newtype StdoutTrim a = StdoutTrim {fromStdoutTrim :: a}

-- | Collect the ``stderr`` of the process.
--   If used, the ``stderr`` will not be echoed to the terminal, unless you include 'EchoStderr'.
newtype Stderr a = Stderr {fromStderr :: a}

-- | Collect the ``stdout`` and ``stderr`` of the process.
--   If used, the ``stderr`` and ``stdout`` will not be echoed to the terminal, unless you include 'EchoStdout' and 'EchoStderr'.
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
Traced String -- ^ Name to use with 'traced', or ``\"\"`` for no tracing. By default traces using the name of the executable.
Timeout Double -- ^ Abort the computation after N seconds, will raise a failure exit code. Calls 'interruptProcessGroupOf' and 'terminateProcess', but may sometimes fail to abort the process and not timeout.
AutoDeps -- ^ Compute dependencies automatically. Only works if 'shakeLintInside' has been set to the files where autodeps might live.
UserCommand String -- ^ The command the user thinks about, before any munging. Defaults to the actual command.
FSAOptions String -- ^ Options to ``fsatrace``, a list of strings with characters such as ``\"r\"`` (reads) ``\"w\"`` (writes). Defaults to ``\"rwmdqt\"`` if the output of ``fsatrace`` is required.
NoProcessGroup -- ^ Don't run the process in its own group. Required when running ``docker``. Will mean that process timeouts and asyncronous exceptions may not properly clean up child processes.

EchoCommand Bool -- ^ Print each command to stdout before it is executed. We call this echoing because it gives the appearance that you are typing the lines yourself.

-v, --verbose
  show all command lines while building, as if all recipes had EchoCommand True

‘-s’ ‘--quiet’
    Quiet operation; do not print the commands as they are executed, as if all recipes had EchoCommand False.

IgnoreExitStatus Bool -- ^ when false: If there is an error (the exit status is nonzero), throw an error and stop executing the task.when True: print exit status if non-zero and continue execution.

‘-i’ ‘--ignore-errors’
    Ignore all errors in commands, as if all recipes had IgnoreExitStatus True.

--skip-commands, RunCommands :: Bool
  Default to 'True'. Set to 'False' to skip all command line actions (treat each command as an operation that does nothing, produces no output on stdout/stderr, and returns a 0 exit code). Useful for profiling the non-command portion of the build system.

Querying the build graph
------------------------

The build graph defines how to tell whether a task needs recompilation, and the entry point to update the task. But running the task is not always what you want; sometimes you only want to know what would be run.

‘-n’
‘--dry-run’

    “No-exec”. Print the tasks that would normally execute to make the targets up to date, but don't actually execute them or modify the filesystem. This is implemented by processing the output from the simulation; certain to execute, likely to execute, certain to substitute, likely to execute but possible to substitute, likely to be skipped. This flag is useful for finding out which tasks Cot thinks are necessary without actually doing them.

‘-q’
‘--question’

    “Question mode”. Silently check whether the targets are up to date. Do not run any recipes, or print anything; just return an exit status code that is zero if the specified targets are already up to date, one if any updating is required, or two if an error is encountered. This is implemented by running as normal but aborting if a task is actually executed.

Forcing/avoiding recompilation
------------------------------

if your build system is broken then you can't fix it with the ``touch`` utility. so a command ``--touch`` that forces files to be invalid seems necessary, although it wouldn't be needed normally.

‘-t’
‘--touch’

    Touch files - mark the build as up to date without actually running it, pretending that the build was done but no output files changed, in order to fool future invocations of make. make walks through the build graph and modifies each initial filesystem input recorded in a task record to match the state from the filesystem. The name of the modified task is also printed, ``touch $task``, unless ‘-s’ or .SILENT is used. Note that intermediate or output files are not recorded, so they will still appear as damaged if they are modified and touch is run.

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

When an error happens that propagates out of the task, it implies that the current task cannot be correctly remade, and neither can any other task that is chronologically after. No further tasks will be executed after the task, since the preconditions have not been achieved.

If a recipe fails (is killed by a signal or exits with a nonzero status), and errors are not ignored for that recipe (see Errors in Recipes), the remaining recipe lines to remake the same target will not be run. If a recipe fails and the ‘-k’ or ‘--keep-going’ option was not given (see Summary of Options), make aborts execution. If make terminates for any reason (including a signal) with child processes running, it waits for them to finish before actually exiting.


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
   similar to ``make --keep-going``.

Normally make gives up immediately in this circumstance, returning a nonzero status. However, if the ‘-k’ or ‘--keep-going’ flag is specified, make continues to consider the other prerequisites of the pending targets, remaking them if necessary, before it gives up and returns nonzero status.

Normally, when an error happens in executing a shell command, make gives up immediately, returning a nonzero status. No further recipes are executed for any target. The error implies that the goal cannot be correctly remade, and make reports this as soon as it knows.

When you are compiling a program that you have just changed, this is not what you want. Instead, you would rather that make try compiling every file that can be tried, to show you as many compilation errors as possible.

On these occasions, you should use the ‘-k’ or ‘--keep-going’ flag. This tells make to continue to consider the other prerequisites of the pending targets, remaking them if necessary, before it gives up and returns nonzero status. For example, after an error in compiling one object file, ‘make -k’ will continue compiling other object files even though it already knows that linking them will be impossible. In addition to continuing after failed shell commands, ‘make -k’ will continue as much as possible after discovering that it does not know how to make a target or prerequisite file. This will always cause an error message, but without ‘-k’, it is a fatal error (see Summary of Options).

The usual behavior of make assumes that your purpose is to get the goals up to date; once make learns that this is impossible, it might as well report the failure immediately. The ‘-k’ flag says that the real purpose is to test as much as possible of the changes made in the program, perhaps to find several independent problems so that you can correct them all before the next attempt to compile. This is why Emacs’ M-x compile command passes the ‘-k’ flag by default.

For example, after an error in compiling one object file, ‘make -k’ will continue compiling other object files even though it already knows that linking them will be impossible.
The usual behavior assumes that your purpose is to get the specified targets up to date; once make learns that this is impossible, it might as well report the failure immediately. The ‘-k’ option says that the real purpose is to test as many of the changes made in the program as possible, perhaps to find several independent problems so that you can correct them all before the next attempt to compile. This is why Emacs’ compile command passes the ‘-k’ flag by default.

Usually when a recipe line fails, if it has changed the target file at all, the file is corrupted and cannot be used—or at least it is not completely updated. Yet the file’s time stamp says that it is now up to date, so the next time make runs, it will not try to update that file. The situation is just the same as when the shell is killed by a signal; see Interrupts. So generally the right thing to do is to delete the target file if the recipe fails after beginning to change the file. make will do this if .DELETE_ON_ERROR appears as a target. This is almost always what you want make to do, but it is not historical practice; so for compatibility, you must explicitly request it.

Creating a build system
=======================

Initially a build system starts out as a list of commands. Then when we trace the commands, the list becomes a partially ordered set of commands because we can relax the ordering to write-read constraints. Then we abstract the commands, adding in-memory keys for configuration changes such as the command line, task arguments to share command handling logic, and a nesting relation for which tasks call which other tasks.

To make using a self-hosting build system installable, there should also be a way to output a list of commands that implement the system.

