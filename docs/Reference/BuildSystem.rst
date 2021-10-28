Build system
############

Stroscot's build system is called "Cot".

Tasks
=====

Cot structures the build into units of computation called tasks. Tasks are the smallest unit of execution addressable by the build system and constrain the amount of re-use that an incremental build can achieve. A task reads and writes shared state and then finishes with a synchronization operation. Synchronization operations enforce control dependencies and structure the flow of time in the program. Usually one task maps to one system command, but there may also be no-op tasks used for structuring the build via synchronization operations, or multiple commands may be run from one task if the intermediate state does not need to be cached.

Each task is required to have a name, which can be any sequence of bytes. Any naming scheme may be used, but there is a distinguished initial task named "Start". Each task only runs once during the build.

Scheduling
==========

Cot can execute several tasks at once. This is implemented using a resource system; by default each task consumes one "CPU" resource and there are as many CPU resources as there are physical processors. But you can specify the number of CPUs consumed and also define other resources - in general a task runs with a multiset of resources.

Each task also has a priority (an integer) - higher priorities run first. Ties are broken by adjoining the priority with a random number and an incrementing ID.

Keys
====

Cot provides types of keys.

Wanted
------

Sometimes we want to always do an action. In such a case we can use the wanted key - the task will run on every build. We say the task is a wanted task.

Wanted tasks can be used for input - e.g. if you need the time the build was made, you can record it in a wanted task. This marks where the build is not reproducible.

Another use is for marking the final outputs. In a build that exactly matches a cached build, the end results aren't used by anything, so by default Cot won't realize them. To fix this, at the end of the build process you should run wanted tasks that read in all the end products. Cot will then ensure that all the wanted outputs are up-to-date and available for use. From a package system viewpoint, these sorts of wanted tasks can be thought of as defining packages.

Dirty bit
---------

The idea of a dirty bit is that the key is either dirty or clean. At the start of each build/rebuild the key is clean. If a task executes and writes the key, the key becomes dirty. But the key does not become dirty if the task is replayed. A task that reads a dirty key must execute. But a clean key does not force a rerun.

A dirty bit doesn't implement correct incremental rebuilds. But it doesn't require any storage, just a little bit of memory during the build, so it can be useful for interactive systems.

In-memory
---------

In-memory keys store the whole value in the database for every read and write. They are useful for small values such as timestamps and hashes.

Hashed
------

Hashed keys are similar to in-memory keys but intern/hash their value in a separate table, so you can use larger strings without worrying about duplication.

Files
-----

Files store a proxy of the contents for each read and write. The read proxies and write proxies don't necessarily have to match in format. Each read proxy should match the previous write proxy, if there aren't any new file writes.

version number/custom detector
  For toolchains in small projects, the version number from running ``gcc -V`` etc. is often sufficient. Although modtime is more robust, it's worth listing this as an example of a custom file modification detector.

file size/permissions/inode number
  Checking the file size is fast and cheap as it's stored in every filesystem. This catches most changed files, but is incomplete since a modification may keep the same file size. In most cases it isn't necessary to track this as modification time alone is sufficient. File permissions can also be relevant, if they are changed from the default.

modtime/device/inode number
  As opposed to make's simple "is-newer" comparison, storing the full mtime value is pretty accurate. mtime changes at least as often as the content hash changes. There is a small risk that a file archiver or inaccurate clock will set the timestamp to collide with the old one and the change won't be detected. The device/inode number detects replaced files, e.g. if you ``mv`` a file onto another one. The real disadvantage is over-rebuilding, due to ``touch`` and similar. ctime and atime update even more frequently than mtime, so they don't help. btime / creation time might be useful, in a manner similar to inode number. Simply checking all the modtimes sequentially is very efficient due to filesystem caching and it can be made even more efficient with various tricks (parallel threads, maybe grouping by directory).

digest
  A digest computed from the contents. There is a remote risk that the file will change without its digest changing due to a collision, but otherwise this detects changes accurately. The disadvantage of digests is that they are somewhat slow to compute, requiring a full scan of the file. But various virtual filesystems store precalculated file checksums, in which case those would be better to use than mtime. There are fast hash algorithms like `xxHash <https://cyan4973.github.io/xxHash/>`__ that have throughput faster than RAM, so the main bottleneck is the I/O. Looking at the `benchmark <https://github.com/Cyan4973/xxHash/wiki/Performance-comparison>`__, and fruitlessly googling around to find other hashes not listed there (fnv1, murmurhash, siphash), it seems xxHash3 / xxHash128 are the fastest. But, if we are going to share the files over a network then one of the SHA's or BLAKE3 might be better to prevent file-replacement attacks. There is also the Linux Kernel Crypto API using AF_ALG but it seems to be slower than doing it in user-space.

watcher/change journal
  We can run a filesystem watching service like Watchman, on Windows use the `USN journal <https://en.wikipedia.org/wiki/USN_Journal>`__, strace all running programs, or redirect filesystem operations through a FUSE vfs. In each case, we get a list (journal) of all changes since some arbitrary starting point. If the journal covers all of the time since the last build, we have a full list of changes and don't need anything else; otherwise we need to supplement it with one of the other methods. The watcher is much faster than scanning all the files for changes on startup. A large scan without the watcher can take several minutes with a hashing algorithm or a few seconds with modification times, whereas the watcher is almost instant.


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

    On systems that support symbolic links, store and check the timestamps on any symbolic links in addition to the timestamp on the file referenced by those links.

Access Tracing
~~~~~~~~~~~~~~

Access tracing can verify or replace manually specified dependencies. There are various tracing methods:

* library preloading with fsatrace: fails on static linking, Go programs, and Mac system binaries
* ptrace with BigBro-fsatrace: Linux-only at present, might work on Windows/Mac eventually.
* chroot with FUSE: mount real system at ``/real-system/``, FUSE system with all files ``/x`` as symlinks to ``/real-system/x``. The program shouldn't access ``/real-system/`` directly. Handles all programs, even forking/multiprocess programs like make, and gives build system the abilities to hide new files and generate files on-demand. Requires Linux + root.
* modtime checking: a little slow but useful if none of the other methods work. Doesn't work multithreaded.

Network
-------

Often we wish to fetch data from over the network. There are a few common protocols:

* HTTP downloads: we can use wget, curl, aria2, or a custom library. The `caching headers <https://developer.mozilla.org/en-US/docs/Web/HTTP/Caching>`__ are important for re-using old downloads.
* FTP: this can be treated similarly to the filesystem
* Git, Bittorrent, IPFS: these are content-addressed stores so keeping track of the hash is sufficient

A more complex example is deploying a container to AWS. The inputs are: all the configuration details for the host, the container image itself, and secret credential information. The output is a running instance or else a long log file / error message. But the running instance cannot be checksummed, so we must use some proxy criterion - the easiest is to redeploy if any inputs have changed, but we could also use a script to interrogate the running instance over the network.

If there are multiple containers that depend on each other, we have to encode the restarting behavior somehow. The easiest is probably to write a single script that takes all the configuration and starts up the containers in order, but this duplicates the build system task scheduling logic. So a script for each strongly-connected component.

Options
=======

The remaining documentation lists options for the build system. If there is more than one option or options conflict, the last one is effective.

Metadata
========

-m, --metadata $files
    The directory used for storing metadata files. All metadata files will be named ``$files/$filename``. If the ``$files`` directory does not exist it will be created. If set to ``Nothing`` then no metadata files are read or written (clean build mode). Defaults to ``.cot``.

--flush N
    How often to flush metadata files in seconds, or ``--never-flush`` to never flush explicitly. On abnormal termination the completion data that has not been flushed will be lost.

Commands
========

These options control the behavior of "commands", new processes spawned by the build system.

-C dir, --directory=dir
    Change directory to ``dir`` before running each command, as if all commands had ``Cwd dir``.

-v, --verbose
    print all command lines while building, as if all commands had ``EchoCommand True``

-s, --quiet
    Quiet operation; do not print the commands as they are executed, as if all commands had ``EchoCommand False``.

-i, --ignore-errors
    Ignore all errors in commands, as if all commands had ``IgnoreExitStatus True``.

--skip-commands
    Skip all command line actions (treat each command as an operation that does nothing, produces no output on stdout/stderr, and returns a 0 exit code), as if all commands had the option ``Run False``. Useful for profiling the non-command portion of the build system.

Cleaning
========

-c, --clean, --remove

    Clean up by removing the selected targets

--clean-old

    clean built files that are no longer produced by the current build. A bad idea if there are multiple configurations that build different subsets, otherwise quite useful.

Cached build
============

A build cache records the outputs of each task in a reproducible manner, i.e. the trace is constructive in the sense of :cite:`mokhovBuildSystemsCarte2020`. A build can be made reproducible by forcing every non-reproducible task to be loaded from the cache. The cache system can also greatly speed up subsequent builds if not many files are changed.

--cache-enable PATH
  If present, use and store outputs in the given shared directory (which may already exist or not). Cot will retrieve files from the cache and copy files to the cache, subject to other options. The cache path is stored in the metadata for future invocations.

--cache-populate
  When using --cache-enable, populate a derived-file cache by copying any already-existing, up-to-date derived files to the cache, in addition to files built by this invocation. This is useful to populate a new cache with all the current derived files, or to add to the cache any derived files recently built with --cache-ignore.

--cache-ignore, --cache-disable, --cache-delete
  The ignore option can be used to run a single build without the cache. The disable option removes the cache from the metadata without modifying the cache folder. The delete option deletes the cache folder and removes it from metadata.

--cache-readonly PATHS
  Use the cache, if enabled, to retrieve files, but for outputs matching listed path patterns, do not update the cache with any files actually built during this invocation.

--cache-links PATHS
  Use hard links or reflinks to record and replay tasks, instead of copying files. The linked files are set to read-only to avoid inadvertently poisoning the cache. The files and the cache directory must be on the same partition.

--cache-check
  Sanity check the shared cache files.

--cache-cloud URL
  HTTP server providing a (read-only) cache in the cloud.

--cache-trim BYTES
  Delete the least recently used files in the cache until the non-linked files are smaller than BYTES.

--check-reproducible FLOAT
  In case of a cache hit, Cot will rerun the rule anyway with the given probability and compare the results against the cache hit. If the files differ, the rule is not reproducible and the failure will be logged.

Remote Builds
=============

A remote build consists of a local build setup forwarding task invocations to other machines. This allows multiple builds to be performed in parallel and to do multi-platform builds in a semi-transparent way.

cot ping-builders
  Test whether connecting to each remote instance works. To forward a build to a remote machine, it’s required that the remote machine is accessible via SSH and that it has Cot installed. If you get the error ``cot: command not found`` then you need to ensure that the PATH of non-interactive login shells contains Cot.

Each machine specification consists of the following elements, separated by spaces. Only the first element is required. To leave a field at its default, set it to -.

* The URI of the remote store in the format ssh://[username@]hostname, e.g. ssh://builder@mac or ssh://mac. For backward compatibility, ssh:// may be omitted. The hostname may be an alias defined in your ~/.ssh/config. It is possible to specify an SSH identity file as part of the remote store URI, e.g. ``ssh://mac?ssh-key=/home/alice/my-key``. Since builds should be non-interactive, the key should not have a passphrase. Alternatively, you can load identities ahead of time into ssh-agent or gpg-agent, as SSH will use its regular identities.
* The maximum number of builds to execute in parallel on the machine. Typically this should be equal to the number of CPU cores. For instance, the machine itchy in the example will execute up to 8 builds in parallel.
* The “speed factor”, indicating the relative speed of the machine. If there are multiple machines of the right type, Cot will prefer the fastest, taking load into account.
* A comma-separated list of supported features and platform identifiers, such as ``i686-linux,x86_64-linux,kvm``. Cot will only perform the derivation on a machine that has the specified features.
* A comma-separated list of mandatory features. A machine will only be used to build a derivation if all of the machine’s mandatory features appear in the derivation’s features attribute.

Remote builders can be configured on the command line with ``--builders`` or in general conf or in a separate configuration file included in builders via the syntax @file.

builders-use-cache

    If set to true, remote hosts will fetch as many build dependencies as possible from a build cache, instead of upload the files from the host. This can drastically reduce build times if the network connection between this computer and the remote build host is slow. Defaults to false.

To build only on remote builders and disable building on the local machine, you can use the option --max-jobs 0.

Parallel Execution
==================

--random, --random=SEED, --deterministic

    Build dependencies in a random order (the default) or a deterministic order. This is useful to prevent various scheduling slowdowns in the build, and can reduce contention in a build farm.

-j [jobs], --jobs[=jobs]

  Specifies the maximum number of tasks (jobs) that may be running simultaneously.
  For many build systems, a number equal to or slightly less than the number of physical processors
  works well. Use ``detect`` to match the detected number of processors, ``detect-1``, etc.
  ``load`` represents the rounded system load on startup so ``detect-load`` would avoid conflicting with other tasks on the system. ``infinity`` means no limit. Defaults to ``detect``.


Querying the build graph
------------------------

The build graph defines how to tell whether a task needs recompilation, and the entry point to update the task. But running the task is not always what you want; sometimes you only want to know what would be run.

-n, --dry-run

    “No-exec”. Print the tasks that would normally execute to make the targets up to date, but don't actually execute them or modify the filesystem. This is implemented by processing the output from the simulation; certain to execute, likely to execute, certain to substitute, likely to execute but possible to substitute, likely to be skipped. This flag is useful for finding out which tasks Cot thinks are necessary without actually doing them.

-q, --question

    “Question mode”. Silently check whether the targets are up to date. Do not run any recipes, or print anything; just return an exit status code that is zero if the specified targets are already up to date, one if any updating is required, or two if an error is encountered. This is implemented by running as normal but aborting if a task is actually executed.

Overriding recompilation
------------------------

-W, --what-if, --new-file, --assume-new, -B, --always-make, --rebuild=now, --rebuild=force [TASKS/KEYS]
    Rebuild TASKS and tasks depending on KEYS regardless of input state or previous runs. Useful if the build system missed a dependency. May cause other tasks to execute, if the task output is different.

--rebuild=auto, --rebuild=normal [TASKS/KEYS]
    TASKS will run only if inputs have changed or an output has changed and is needed by another task. KEYS will cause a task to rebuild if different from the previous run (default).

-o, --assume-old, --skip, --rebuild=later, --rebuild=cache [TASKS/KEYS]
    For this run only, consider matching TASKS and KEYS as up-to-date - TASKS will not rebuild even if the inputs have changed, and KEYS will not cause a task to rebuild. The trace of the previous run will be used. Future runs will have normal rebuild behavior.

--assume-old-touch, -t, --touch, --skip-forever, --rebuild=never [TASKS/KEYS]
    For each task that could be executed this run, or each task that would be run due to a change in the given key, instead of executing it, modify the trace from the previous run to mark it as up to date. The task will not be executed in future runs until the inputs change. Hence the build may enter a steady state not matching any clean build. However the modified keys are remembered so you can use ``--untouch`` to globally undo the effects of touch.

--new-tasks=stop, --new-tasks=build
    If a task is requested with no trace from the previous run and that matches ``--rebuild=later`` or ``--rebuild=never``, you can either stop the build (default), or the tasks can be built.

Error handling
--------------

-k, --keep-going, -k, --staunch N

    Continue as much as possible, until N tasks fail (default=1, infinity is an option). The task that failed, and those that depend on it, will not be executed, but other tasks can continue. The build may finish early due to lack of tasks. Once N tasks fail, all tasks will be cancelled by sending cancel messages - they can mask the message to finish a command, or kill the command.
