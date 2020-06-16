Build system
############

Although build systems are often an afterthought in programming language design, they interface with the compiler in several areas, so it is better to integrate the build system into the compiler as a library. That way intermediate results such as typechecked/optimized functions can be stored efficiently and rebuilt only when needed.

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
    * Recheck: a subtask /subkey is dirty, might have to rebuild again
    * Missing: never built, needs to be built
* NB: Unlike Shake, tasks are not keys. Task identifiers are serializable as well, but they are in a different namespace. The store maintains dependency lists of key and task identifiers for each task, but they do not store versions the way keys do.
* Next we go down from the top-level task. We want a suspending build system :cite:`mokhovBuildSystemsCarte2020`. So there must be some way to suspend the current task when it calls a sub-task, probably just continuations like how Shake does it.
* When a task is called, we first check its state to determine whether it needs to be re-run. Missing tasks are run immediately. Present tasks can be skipped immediately. Otherwise we run through the serialized dependency list and re-check the keys / subtasks in order (and in parallel if the subtasks are parallel).
* Before re-running a task, we delete all its generated keys (outputs). After running a task we store its (keyed) outputs with either verifying or constructive traces.
* For the task graph, we have two nontrivial requirements for soundness, similar to :cite:`erdwegSoundOptimalIncremental2015`:

    * There can be at most one task providing a given key.
    * If a task depends on a generated key, it first depends (directly or transitively) on the task providing the key.
* To prune the store, we can do as above and also load all the subtasks of present tasks. Then anything not loaded is not needed and can be pruned, although if there are multiple configurations etc. then this is a bad idea.