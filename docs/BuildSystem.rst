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

An easy way to ensure these last two is to construct a function mapping from generated files (keys) to tasks, and then have a library function for requiring keys which uses the map to require the task and then the key. Unfortunately in a dynamic build such a direct map is not always available and so the requirement is relaxed to allow indirect dependencies. For example, we may have a generated file that is picked up in a search path directory listing. To deal with this directly we would need to introduce build logic into the search mechanism, but a phase separation handles it too with minimal changes. And since dependencies can be required after execution, we can speculatively generate files and require only the ones that are actually used.

Giving tasks versions is a good idea; this amounts to adding a version key as a dependency.

Without an initial list of changed keys, we will have to walk the whole graph. This can still be done efficiently by batching filesystem reads. A bigger question is whether up-propagation of dirtyiness can be avoided. My intuition is that most dependency graphs are tree-like and so going up is roughly :math:`\log(n)`. There are some dependencies (e.g. small common functions) which have a huge reverse dependency list, but changing those requires a full rebuild anyway so the overhead is dwarfed.

Package manager
===============

A language also needs a package manager. Compared to a build system alone, the main feature is downloading files over the network and verifying cryptographic hashes. In the build graph a new state "Substituted" is added which behaves similarly to "Present"; when a task is requested, and package management is enabled, the task is checked against a list of prebuilt tasks and if so the relevant keys are downloaded instead of the task being built. Relevant here means the runtime dependencies needed by other tasks. From a build perspective this is a shortcut, as running the task should recreate everything the task builds to avoid potentially leaving out necessary files, but most people who use a package manager do not rebuild their intermediate steps and want the smallest possible package sizes.

In the implementation details are some filesystem issues, in particular different layouts on different systems and allowing per-user installs, but Conda has worked out a reasonable layout.

Linux distribution
==================

Once we have a package manager we can build a Linux distribution. Compared to a user-level package manager, a system-level package manager must be built a bit more robustly to handle crashes/rollbacks. It also needs various build system hooks for dealing with tricky/non-standardized installation procedures, e.g. putting kernel/initrd images into the boot manager, building in a container with overlayfs to guard against untrustworthy packages, and using auditd to identify file dependencies in a bulletproof manner. As a basis for the distribution we can use small distros like LFS and Buildroot. It would also be good to figure out some way to import data from bigger distributions like Arch, Gentoo, or NixOS. Cross-compilation is a goal, but it isn't strictly necessary and it's easily broken anyways.

The goal of the Linux distribution, compared to others, is automation: all package updates are automatic, and packaging new software is as simple as giving a package identifier / URL (and dependency information or build instructions, for C/C++ projects or custom build systems). Language-specific package repositories have grown to be bigger than most distros, so providing easy one-line installation of them is paramount.

CI/CD
=====

Along with a Linux distribution (or any large software collection) comes the need to continuously test and update packages. Besides providing a prebuilt collection of packages, the main purpose is to identify breakages, i.e. when a test or build fails due to an update. Once a breakage is identified, it can be ameliorated by pinning the package to the old version. If the update is in a library and only a few packages are broken by the dependency update, the package can be split into multiple versions and the pin can be restricted to those specific dependencies.

Unfortunately, detecting breakages is an imperfect science; there are exponentially many combinations of different versions, and tests can be flaky. So we can only identify updates that have a high probability of causing a breakage. But, given a breakage, we can use the dependency graph traces to narrow a failure down to a specific build task, so most of the graph can be ruled out immediately and skipped during a rebuild.

We can model the test process as follows:

::

  broken = false
  for change in observed_changes:
    change_type <- choice([broken ? FIXING : BREAKING, NONE], broken, change)
    if change_type = BREAKING:
      broken = true
    else if change_type = FIXING:
      broken = false

    for run in observed_runs:
      flaky <- choice([YES, NO], broken, change)
      if flaky = YES:
        report(!broken)
      else:
        report(broken)

The choice function can be an arbitrarily complicated function of ``commit``, but since the outcome is a random binary we can distill it down to two probabilities for each commit :math:`k`: fixing :math:`P(f_k)` and breaking :math:`P(b_k)`. We'll want complex models to predict these, like the logistic models from :cite:`najafiBisectingCommitsModeling2019` that use the list of files changed / modified components, presence of keywords in commit message, etc. Regardless, our model boils down to a hidden Markov process with two states, broken and working. Since the state space is so small we probably want to work with the second-order process, so we can easily identify breaking and fixing commits. The initial state is known to be working.

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

The table only has one probability, treating the build as a unit; we could also make one table for each test and a UI to aggregate them somehow. From this table, we can make simple decisions, reporting breakages, hiding flaky runs, blacklisting broken builds, etc. once a certainty threshold is reached. But a more important question is determining which build to do next. There are several goals:

* Identifying breakages etc. sufficiently to generate reports
* Minimizing redundant builds/tests that generate no new information
* Redoing runs that are affected by flaky tests
* Doing cheap tests and sensitive tests before others

The general problem falls under "stochastic scheduling". For the optimal (intractable) solution, as well as the success/failure probability we must also know the cost associated with running the tests (in time/resource usage) and (for parallelisation) the available resources; it is almost a multi-armed bandit problem where getting enough information to generate a report results in a payoff. However, a failing build for one commit changes the calculated probabilities of the other commits failing, so it is not independent.

A simple heuristic is to find the build with ``P(Broken)`` closest to 50%; this ignores flakiness. What we want is to maximize the information entropy gained from a run :math:`X`, i.e.

.. math::

  H(X) = - P(x_s) \log(P(x_s)) - P(x_f) \log(P(x_f))

where :math:`x_s = 1 - x_f` is the probability that the run will succeed.

There is also the issue of spacing out parallel builds to do trisection / general :math:`n`-section. Speculative building will end up doing bisection of bisection (i.e. quadrisection and general :math:`2^n`-section) in the case of little information, which is probably good enough.
