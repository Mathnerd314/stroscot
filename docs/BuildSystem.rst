Build system
############

Although build systems are often an afterthought in programming language design, they interface with the compiler in several areas, so it is better to integrate the build system into the compiler as a library. That way intermediate results such as checked/optimized functions can be stored efficiently and rebuilt only when needed. Also, it allows the compiler's include-following mechanism to tightly integrate with the build system, so that generated files can be generated before they are used.

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

The pipeline of a build system is as follows:

* We start with a changelist of "pending" keys, i.e. keys that might have changed. The prototypical example is a list of changed files from a file-watching daemon, but it can also include volatile information such as external tool version numbers or FTP server listings or finer keys such as individual AST nodes. We could also use the list of all keys from the previous build, skipping the watcher altogether.
* We go through the list and scan the data for each key. If a key has actually changed, we mark all the tasks that have used it as dirty.
* We also propagate dirtiness up the pre-built task/key graph; every task that depends on a dirty task is marked as needing a recheck.
* After scanning all the keys, we go down starting from the top-level task. (We could start the build earlier by doing speculative execution, but scanning is cheap) We want a suspending build system :cite:`mokhovBuildSystemsCarte2020`. So there must be some way to suspend the current task when it calls a sub-task, probably just continuations like how Shake does it.
* When a task is called, we first check its state to determine whether it needs to be re-run. Dirty tasks are run immediately. Loaded tasks can be skipped immediately, as can tasks stored in the database that have not yet been marked. Otherwise, for rechecks, we run through the serialized dependency list and re-check the keys / subtasks in order (and in parallel if the subtasks are parallel). When the task is finished its state is marked as loaded / error.
* Before running a task, we clean up old build results, if any, i.e. delete all generated keys (outputs) that are still present. After running a task we store its (keyed) outputs with either verifying or constructive traces.
* To prune the store (which is a bad idea if there are multiple configurations that build different subsets), we can do as above and also load all the subtasks of present tasks. Then anything not loaded is not needed and its files etc. can be deleted.

Notes
=====

Unlike Shake, tasks are not keys; it is a two-level graph like Pluto. Task identifiers are serialized to the databased as well as keys, but they are in a different namespace. The store maintains dependency lists of key and task identifiers for each task, but tasks do not store versions the way keys do.

For the task graph, we have some nontrivial requirements for soundness, similar to :cite:`erdwegSoundOptimalIncremental2015`:

* The graph is a DAG.
* There can be at most one task providing a given key.
* If a task depends on a generated key, the task providing the key must have been run first.

An easy way to ensure these last two is to construct a function mapping from generated files (keys) to tasks, and then have a library function for requiring keys which uses the map to require the task and then the key. Unfortunately in a dynamic build such a direct map is not always available and so the requirement is relaxed to allow indirect dependencies. For example, we may have a generated file that is picked up in a search path directory listing. To deal with this directly we would need to introduce build logic into the search mechanism, but a phase separation handles it too with minimal changes. And since dependencies can be required after execution, we can speculatively generate files and require only the ones that are actually used.

Giving tasks versions is a good idea; this amounts to adding a version key as a dependency.

Without an initial list of changed keys, we will have to check all the keys individually. This can still be done efficiently by batching filesystem stat's using io_uring (`4x-8x faster <https://twitter.com/axboe/status/1205991776474955777>`__). A bigger question is whether up-propagation of dirtyiness can be avoided. The intuition is that most dependency graphs are tree-like and so going up is roughly :math:`\log(n)`, which seems acceptable. There are some dependencies (e.g. small common functions) which have a huge reverse dependency list, but changing those requires a full rebuild anyway so the overhead is dwarfed, and the changes might not propagate up the tree.

Package manager
===============

A language also needs a package manager. Compared to a build system alone, the main feature is downloading files over the network (like wget, curl, aria2, etc.) and verifying cryptographic hashes/signatures. When a task is requested, and package management is enabled, the task is checked against a list of prebuilt tasks and if so all of the task's generated keys (files) are downloaded instead of the task being built.

The list of generated files can be kept accurate by a filesystem access tracer or restricting the build scripts. A tracer will also pick up source files, intermediate object files, etc., but most people who use a package manager do not rebuild their intermediate steps and want the smallest possible package sizes. So we need some way to mark these scratch files; the easiest requirement is that the task delete all the junk data, as packaging a nonexistent file/directory is simply verifying that it doesn't exist on the target system.

There are also some filesystem convention/naming issues, in particular different layouts on different systems and allowing per-user installs, but Conda has worked out reasonable solutions for these, relative pathhs and so on.

A useful feature not implemented in most package managers is P2P distribution, over Bittorrent or IPFS. Trust is an issue in theory, but in practice only a few nodes provide builds so a key ring is sufficient. Turning each tarball into a torrent file / IPFS CID and getting it to distribute is not too hard, the main issue seems to be scaling to thousands of packages as DHT performance is not too great (Bittorrent is `not too great <https://wiki.debian.org/DebTorrent#line-42>`__). There are some notes `from IPFS <https://github.com/ipfs-inactive/package-managers>`__ and various half-baked package managers like ``npm-on-ipfs``.

Linux distribution
==================

Once we have a package manager we can build a Linux distribution. Compared to a user-level package manager, a system-level package manager must be built a bit more robustly to handle crashes/rollbacks. It also needs various build system hooks for dealing with tricky/non-standardized installation procedures, e.g. putting kernel/initrd images into the boot manager, building in a container with overlayfs to guard against untrustworthy packages, and using auditd to identify file dependencies in a bulletproof manner. As a basis for the distribution we can use small distros like LFS and Buildroot. It would also be good to figure out some way to import data from bigger distributions like Arch, Gentoo, or NixOS. Cross-compilation is a goal, but it isn't strictly necessary and it's easily broken anyways.

The goal of the Linux distribution, compared to others, is automation: all package updates are automatic, and packaging new software is as simple as giving a package identifier / URL (and dependency information or build instructions, for C/C++ projects or custom build systems). Language-specific package repositories have grown to be bigger than most distros, so providing easy one-line installation of them is paramount.

Package pinning is an issue, to handle broken software and stale dependencies. A new release of a tool might just not work; then it needs to pinned to the old version. In contrast, a library update might break only a few packages; the distro should then package multiple versions of the library and build most packages with the new libary while pinning the library to the old version for the specific breakages. On normal distros this would be accomplished using soname separation, ``libf.so.1`` vs ``libf.so.2``, but this is pretty fragile compared to using a full package hash. Detecting ABI changes to generate the versions can be automated but it isn't pretty.

Release monitoring
==================

Automating package updates requires finding new releases and then testing it. For the first part, unfortunately there is no standardized API. There is `Anitya <https://fedoraproject.org/wiki/Upstream_release_monitoring>`__, which solves some of this, and also `cuppa <https://github.com/DataDrake/cuppa>`__. But both of them work by writing backends/providers for each major hosting site. We can write our own:

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

* Feed: A way to efficiently get a list of package updates (in particular an RSS feed or Git repo)
* Index: A compressed list of all the packages and their versions (Git repo, ``ls-lR``, rsync)
* Versions: For a package, a list of its available versions


Automation
==========

Along with a Linux distribution (or any large software collection) comes the need to continuously test and update packages. An automation system handles several tasks:
* Pulling together new changes
* Testing changes and identifying breakages
* Generating reports
* Uploading a nightly release

Since our goal is automation, we want the detection of breakages to be automated as well. Detecting breakages is an imperfect science: there are exponentially many combinations of different changes, and tests can be flaky. So in general we can only identify updates that have a high probability of causing a breakage. The problem falls under "stochastic scheduling", in particular determining which subset of changes to schedule a build for, given uncertain information about build successes/failures.

The general goal is to minimize the time/build resources needed for identifying breakages, i.e. to maximize the information gained from each build. Incremental building means that the most efficient strategy is often building in sequence, but this does not hold for larger projects where changes are almost independent.

Regarding the ordering of changes, oftentimes they are technically unordered and could be merged in any order. But an optimized order like least likely to fail could lead to arbitrarily long merge times for risky changes. It is simpler to do chronological order. This could be customized to prioritize hotfixes before other changes, but it is easier to set up a dedicated scheduler for those.

To handle breakages, there are two main strategies: marking and backouts. Both are useful; a test failure may be unimportant or outdated, suggesting the marking strategy, while backouts reject bad changes from the mainline and keep it green. Backouts are harder to compute: for :math:`n` commits, there are :math:`2^n` possible combinations to test, giving a state space of size :math:`2^{2^n}`. Meanwhile marking only has :math:`2^n` states. Marking is run over already-committed changes, hence must often deal with the entire commit history, while backouts are for pending changes and only need to consider a subset of commits.

Marking
-------

For marking, we can model the test process as follows:

::

  broken = false
  for change in changes:
    change_type <- choice([broken ? FIXING : BREAKING, NONE], broken, change)
    if change_type = BREAKING:
      broken = true
    else if change_type = FIXING:
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
The table treats the build as a unit; for added precision we could also make one table for each test and a UI to aggregate them somehow. From this table, we can make simple decisions, reporting breakages, hiding flaky runs, blacklisting broken builds, blessing working revisions, etc. once a certainty threshold is reached.

A simple heuristic for the next build is to find the build with ``P(Broken)`` closest to 50%; this ignores flakiness. What we want is to maximize the Kullback-Leibler divergence / `information gain <https://en.wikipedia.org/wiki/Information_gain_in_decision_trees>`__ from a run :math:`X`, i.e. something like

.. math::

  H(X) = - P(x_s) \log(P(x_s)) - P(x_f) \log(P(x_f))

where :math:`x_s = 1 - x_f` is the probability that the run will succeed. To accommodate differing build costs we can simply divide by the cost; it works for Bayesian search of boxes so it probably works here.

Overall, the idea is similar to ``git bisect``'s ``min(ancestors,N-ancestors)``, but with more advanced models and using expectation instead of ``min``. To implement a full regression tool we also need to mark and handle untestable revisions, where the test is not observable due to the build being broken etc. This is fairly straightforward and amounts to doubling the state space and adding some more probability models.

Backouts
--------

For backouts, we must first decide a backout strategy. We should maximize the number of commits included, but this alone is not enough to decide between ``A,B`` and ``A,C``; we might as well prefer the earlier commit ``A,B``. Also, for ``A`` vs ``B,C``, to get ``B,C`` we would have to decide to test without ``A`` even though it succeeds. Since ``A`` could already been pushed to mainline this is unlikely. So we instead have early-biased lexicographic preference: we write ``A,B`` and ``B,C`` as binary numbers ``110`` and ``011`` and compare them.

The paper :cite:`ananthanarayananKeepingMasterGreen2019` assumes accurate build results and that there are no fixing commits, i.e. if ``A`` fails then ``A,B`` will fail as well. But in general this isn't true; we need a more complex model accounting for breakages, fixes, dependencies, conflicts, and flakiness. But we'll assume no higher-order phenomena, e.g. fixes to conflicts.

::

  breaking = []
  for c in changes:
    is_breaking <- choice([YES, NO], c)
    if is_breaking:
      breaking += c

  fixing = {}; fixing.default = []
  for c2 in changes:
    for c in breaking:
      if c2 <= c:
        continue
      is_fixing <- choice([YES, NO], c, c2)
      if is_fixing:
        fixing[c2] += c

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

    outer:
    for b in breaking:
      if !set.contains(b)
        continue
      for f in fixing[b]:
        if set.contains(f)
          continue outer
      fail_type = BREAKAGE

    for c in set:
      for d in dependencies[c]:
        if !set.contains(d)
          fail_type = DEPENDENCY


    for c2 in conflicts:
      for c in conflicts[c]:
        if set.contains(c)
          fail_type = CONFLICT

    flaky = choice([YES, NO], fail_type)
    broken = fail_type == NONE
    if flaky = YES:
      report(!broken)
    else:
      report(broken)

The size and complexity presents a challenge, but at the end of the day it's just a large Bayesian network, and we want to determine the highest-ranking success, based on the (unobserved/hidden) brokenness properties.
