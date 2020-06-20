Build system
############

Although build systems are often an afterthought in programming language design, they interface with the compiler in several areas, so it is better to integrate the build system into the compiler as a library. That way intermediate results such as typechecked/optimized functions can be stored efficiently and rebuilt only when needed. Also, it allows the compiler's include-following mechanism to tightly integrate with the build system, so that generated files can be generated before they are used.

.. graphviz::

    digraph foo {
        rankdir=LR;
        subgraph cluster_0 {
            style=filled;
            color=lightgrey;
            node [style=filled,color=white];
            a0 -> a1 -> a2;
            a0, a1, a2 [shape="circle"];
            a0 [label="require"];
            a1 [label="cmd"];
            a2 [label="provide"];
            label = "BuildModule C.java";
        }
        "Javac config" -> a0
        "C.java" -> a0
        a2 -> "C.class"
        a2 -> "C$1.class"
        a2 -> "C$Foo.class"
    }

The pipeline of a build system is fairly straightforward:

* We start with a changelist of "dirty" keys; this is usually a list of local files, but can also include volatile information such as version numbers or FTP server listings or finer keys such as individual AST nodes.
* Next we propagate dirtiness up the pre-built task/key graph; this allows us to identify tasks as being in one of these states:

    * Present: built, not dirty
    * Recheck: a subtask / subkey is dirty, might have to rebuild again
    * Missing: never built, needs to be built
* Next we go down from the top-level task. We want a suspending build system :cite:`mokhovBuildSystemsCarte2020`. So there must be some way to suspend the current task when it calls a sub-task, probably just continuations like how Shake does it.
* When a task is called, we first check its state to determine whether it needs to be re-run. Missing tasks are run immediately. Present tasks can be skipped immediately. Otherwise we run through the serialized dependency list and re-check the keys / subtasks in order (and in parallel if the subtasks are parallel).
* Before re-running a task, we delete all its generated keys (outputs). After running a task we store its (keyed) outputs with either verifying or constructive traces.
* To prune the store, we can do as above and also load all the subtasks of present tasks. Then anything not loaded is not needed and can be pruned, although if there are multiple configurations etc. then this is a bad idea.

Notes
=====

Unlike Shake, tasks are not keys; it is a two-level graph like Pluto. Task identifiers are serializable as well, but they are in a different namespace. The store maintains dependency lists of key and task identifiers for each task, but tasks do not store versions the way keys do.

For the task graph, we have some nontrivial requirements for soundness, similar to :cite:`erdwegSoundOptimalIncremental2015`:

* The graph is a DAG.
* There can be at most one task providing a given key.
* If a task depends on a generated key, the task providing the key must have been run first.

An easy way to ensure these last two is to construct a function mapping from generated files (keys) to tasks, and then have a library function for requiring keys which uses the map to require the task and then the key. Unfortunately in a dynamic build such a map is not always available.

Giving tasks versions is a good idea; this amounts to adding a special version key as a dependency.

Without an initial list of changed keys, we will have to walk the whole graph. This can still be done efficiently by batching filesystem reads. A bigger question is whether up-propagation of dirtyiness can be avoided. My intuition is that most dependency graphs are tree-like and so going up is roughly :math:`\log(n)`. There are some dependencies (e.g. small common functions) which have a huge reverse dependency list, but changing those requires a full rebuild anyway so the overhead is dwarfed.
