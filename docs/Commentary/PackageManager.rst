Package manager
###############

A language also needs a package manager. When a task is requested, and package management is enabled, the task is checked against a list of prebuilt tasks and if so all of the task's provided keys (files) are downloaded instead of the task being built, verifying their cryptographic hashes/signatures. We also need a way to create packages from a build tree.

The list of provided files can be kept accurate by a filesystem access tracer or restricting the build scripts. A tracer will also pick up source files, intermediate object files, etc., but most people who use a package manager do not rebuild their intermediate steps and want the smallest possible package sizes. So we need some way to mark these scratch files; the easiest requirement is that the task delete all the junk data, as packaging a nonexistent file/directory is simply verifying that it doesn't exist on the target system. Or we can ensure the build script places all of its provided files in a specific location.

A useful feature not implemented in most package managers is P2P distribution, over Bittorrent or IPFS. Trust is an issue in theory, but in practice only a few nodes provide builds so a key ring is sufficient. Turning each tarball into a torrent file / IPFS CID and getting it to distribute is not too hard, the main issue seems to be scaling to thousands of packages as DHT performance is not too great (Bittorrent is `not too great <https://wiki.debian.org/DebTorrent#line-42>`__). There are some notes `from IPFS <https://github.com/ipfs-inactive/package-managers>`__ and various half-baked package managers like ``npm-on-ipfs``.

In a long-running system, the number of prebuilt packages could grow without bound. We need a mechanism to clean out the archives when space becomes limited.

Edges are bidirectional. To fix the GC problem, we use weak references for back edges, but strong references for memo table entries, so that from the GC’s point of view, all DCG nodes are always reachable. To implement safe space reclamation, we also implement reference counting of DCG nodes, where the counts reflect the number of strong edges reaching a node. When DCG edgesare deleted, the reference counts of target nodes are decremented. Nodes that reach zero are not immediately collected; thisallows thunks to be “resurrected” by the swapping pattern. Instead, we provide aflushoperation for memo tables that deletesthe strong mapping edge for all nodes with a count of zero, which means they are no longer reachable by the main program.Deletion is transitive: removing the node decrements the counts of nodes it points to, which may cause them to be deleted.An interesting question is how to decide when to invokeflush; this is the system’seviction policy. One obvious choice is toflush when the system starts to run short of memory (based on a signal from the GC), which matches the intended effect of theunsound weak reference-based approach. But placing the eviction policy under the program’s control opens other possibilities,e.g., the programmer could invokeflushwhen it is semantically clear that thunks cannot be restored. We leave to future work further exploration of sensible eviction policies

Package format
==============

One advantage of C/C++ is that it is the de-facto standard, so everyone has a method to distribute application binaries compiled in those languages. But this is only a small advantage as there is still no common format: there are self-extracting installers and then a variety of package formats. MSI and PKG are fiat standards for Windows and Mac, but they have been superseded by the stores and practically for programming people use application-specific package managers that don't interface with the OS level package managers.

Arch/pacman's tar.xz seems fine.

Linux distribution
==================

Once we have a package manager we can build a Linux distribution. Compared to a user-level package manager, a Linux distro package manager must be built a bit more robustly to handle crashes and allow rollbacks without breaking the system. It needs various build system hooks for dealing with tricky/non-standardized installation procedures, e.g. putting kernel/initrd images into the boot manager, building in a container with overlayfs to guard against untrustworthy packages, and using auditd to identify file dependencies in a bulletproof manner. As a basis for the distribution we can use small distros like LFS and Buildroot. It would also be good to figure out some way to import data from bigger distributions like Arch, Gentoo, or NixOS. Cross-compilation is a goal, but it isn't strictly necessary and it's easily broken anyways as few people use it.

The goal of the Linux distribution, compared to others, is automation and speed. The package update process will be completely automated, including checking for releases, building, and testing. And this process should be fast so new versions of packages become widely available within 24 hours of release. This allows responding to security updates with the normal update cycle.

Also, language-specific package formats and repositories have grown to be bigger than most distros, so providing easy installation of these kinds of packages is paramount. They should install via an identifier ``pypi:pdfminer.six`` or URL ``https://pypi.org/project/pdfminer.six/`` and not need any manual build instructions. Of course C/C++ projects will still need a machine-readable version of the dependency information and custom build systems will need manual build scripts to interface with them.

Upgrade cycle
=============

A package has various versions. It also depends on other packages which can themselves be various versions.

In a perfect world we would simply use the latest "blessed" version of each package and they would all be compatible with each other. But there will inevitably be incompatible packages. Automated testing and manual marking will produce a list of breakages, of the form "combination A-2 B-2 C-2 fails".

Breakages can be resolved in two ways: either release new package versions that are compatible, or use old versions that work together. New versions are long-term the best solution but require patch-writing, so cannot be automated. Hence in the short term the only solution for a distro is to package old versions.

Packaging old versions can be handled in two ways. The traditional way is that a dependency can only have one installed version, and we have to use a solver to find a version that is compatible with all applications. But often there is no solution - e.g. ``python`` cannot be both Python 2 and Python 3, so the packages using Python are split into two incompatible sets and one cannot mix them on a system. With side-by-side dependencies, the dependency lookup is modified to use additional data so that one application can use version A of a dependency and another application can use version B of a dependency in the same installation. E.g. applications lookup ``python2`` and ``python3``.

Versioned paths
===============

There are some filesystem convention/naming issues. Clearly the distro should package multiple versions of various libraries. The key question is where to store them.
For a basic path like ``/usr/share/foo/img.jpg``, we can put a hash ``HASH`` in various places:

1. ``/usr/lib/libfoo.so.HASH.1`` or ``/usr/lib/libfoo.so.1.HASH`` (filename version)
2. ``/usr/lib/HASH/libfoo.so.1`` ("multiarch" layout similar to Debian)
3. ``/usr/HASH/lib/libfoo.so.1`` (NixOS layout)
4. ``/HASH/usr/lib/libfoo.so.1`` ("multisystem" layout)

The multisystem layout isn't useful, as the point of ``/usr`` is to allow putting system files on a separate partition. Also the root directory would become cluttered with all the hashed files.

The filename solution breaks down with data files. Maintaining hashed file versions like  would require patching every application to look things up in the right place. We can move it up to the package directory, ``/usr/share/foo-HASH/foo.jpg``. But autoconf only has the option ``--datarootdir`` to change ``/usr/share``; it doesn't have a standard option to rename the subdirectory. So once again we'd have to manually patch every package. The only feasible option is to move it up more, ``/usr/share/HASH/foo/foo.jpg``. But that's the multiarch layout. So for data files only the multiarch and NixOS layouts are feasible. Comparing them, the NixOS layout has the advantage of putting every package in its own directory, so for example we can find the documentation for a package as ``<path of executable>/../share/something``. With split outputs, this is not as much a benefit to the user, because the documentation will be in a separate package and hence not findable by just browsing the package directory. Here the multiarch layout shows promise as the different sub-packages match up with the directory they unpack to. We can change the various `autoconf directories <https://www.gnu.org/prep/standards/html_node/Directory-Variables.html>`__ by appending ``/HASH`` and leave the rest up to the package; it may install things to ``/usr/$hash/`` if it's not well-written, but everything respects ``$PREFIX``.

For multiarch/NixOS the hash can be put in the SONAME by linking with absolute paths (or relative paths, they would work too). There is `some work <https://github.com/NixOS/nixpkgs/issues/24844>`__ in NixOS to do so. The rpath solution that NixOS uses currently is slow and doesn't solve the diamond problem.

Since the package manager controls all versioning, we want to hardcode the versions and paths of binaries if possible, for minor sanity and efficiency gains. For the cases where this isn't possible,  allowing dynamic resolving of binary names ``foo`` to paths ``/usr/bin/12345/foo`` is not trivial. A global view doesn't work because we could have two binaries who call different versions of a binary. Instead we could make a pseudo-filesystem like devfs or ``/proc`` but for the system path; this can provide the necessary pid-dependent view as a symlink tree ``/system-path/foo -> /usr/bin/foo-12345``; even FUSE should be sufficiently fast since it is just one ``open()`` call and it doesn't have to handle the actual I/O. Currently NixOS uses environment variables, global symlinks in `/run/current-system/`, and chroot containers.


per-user installs: Conda has worked out reasonable solutions for these, relative paths and so on.

Side-by-side C libraries
========================

.. graphviz::

  digraph foo {
    rankdir=LR;
    A -> B;
    A -> C;
    B -> L [label="v1"];
    C -> L [label="v2"];
  }


Solving the diamond dependency problem is tricky but possible. Shared libraries support symbol versioning, which essentially changes the name of each symbol so they don't conflict. The ``--default-symver`` option sets the version string of each symbol to the SONAME of the library it is exported from. So if we include a hash in the SONAME and build with ``--default-symver`` then the libraries won't conflict. Versions aren't linear in general so it has to be a hash instead of a sequential number. The SONAME can be set with a linker / libtool wrapper.

There are two symlinks, the library symlink ``libfoo.HASH -> libfoo.HASH.1`` and the development symlink ``libfoo.so -> libfoo.HASH`` which tells which version to link. ldconfig should create these normally. Prebuilt binaries can be patchelf'd using ``--replace-needed``.

Another solution is to create a manifest that specifies where to load libraries from, but this is basically the same as specifying absolute paths.

Updates
=======

For seamless updates it seems worthwhile to use an `A/B partition scheme <https://source.android.com/devices/tech/ota/ab>`__. There are roughly 3 types of updates:
* small updates that just update a user-level application
* large updates that affect components such as the desktop manager, WiFi, etc.
* kernel / initrd updates

For small updates we want fast rebootless updates in-place and an easy way to rollback the application. But the update won't break the system so providing the rollback functionality via the package manager doing another update is fine. We do need some way to store/manage reproducible configurations though.

For large updates the user's ability to access the package manager may be impaired, so we do need to make the last-known-good-configuration snapshot. In particular there needs to be a boot entry that the user can select to rollback after they hard-reset their computer.

Kernel updates require a reboot or [kexec](https://github.com/NixOS/nixpkgs/issues/10726), but they are otherwise large updates.

Automation system
=================

Although a distribution is sufficient for setting up a single computer, to set up multiple computers it is more complicated. Salt provides a command-execution agent, but the commands are not idempotent. We want a map from packages to their latest versions or pinned versions. The 'autoremove' option is on by default because packages being secretly installed is a bad idea. But with autoremove off, packages are left installed on the system if they aren't explicitly specified for removal.

Release monitoring
==================

Automating package updates requires finding new releases and then testing it. For the first part, there is almost a standardized API. There is `Anitya <https://fedoraproject.org/wiki/Upstream_release_monitoring>`__, which solves some of this, and also `cuppa <https://github.com/DataDrake/cuppa>`__. But both of them work by writing backends/providers for each major hosting site. There is also Repology which checks the various distributions for new versions.

Although the most recently modified / created version is usually the latest release, and hence it is easy to identify, some projects maintain multiple versions, so that newer files might actually be security updates to old versions rather than the latest version. This requires some per-package version handling logic.

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
* Git/Hg/other VCS: We can fetch the tags with git/hg/etc.
* Folder: We can scrape the standard default Apache directory listing
* Projects with version number: scrape the download page
* Projects without versioning: Use HTTP caching to poll the URL. Although, for such isolated files, there is the issue of the license changing suddenly, so the download page is worth watching too.

Overall, there are only a few mechanisms:

* A list of package updates since some time (RSS feed, Git repo)
* A list of all the packages and their versions (Git repo, ``ls-lR``, rsync)
* A list of a single package's available versions (scraping, some package repositories)

For each top-level project, figuring out when/if there will be a new update is a machine learning problem. The simplest algorithm is to poll everything at a fixed interval, say daily. But most projects release a lot less frequently, and some projects (software collection, main development branches) release more frequently. If there is a push service like email we can use that, otherwise we need some sort of adaptive polling. We can model it as a homogeneous Poisson point process; then the estimate for the rate is simply the number of updates divided by the time interval we have observed. Then the time between two updates is an exponential distribution with parameter the rate, so we can poll if the probability of an update is > 50%, adjusting the 50% so we poll an average of once a day. To get even more complex, we can build a feature vector classifier to predict the time between events.

Build scripts
=============

To obtain an initial build script set we can do the following:

1. Evaluate Nixpkgs (nix-instantiate) in a fresh Nix store
1. Change all .drv from ATerm to JSON for ease of processing
1. Assemble a mega pseudo-JSON of all the properties and values in the .drv
1. Rename .drv according to a non-hashed scheme
1. Change fetchurl to a flat list
1. Create a set of builders which covers the rest of the mega-JSON


Automation
==========

Along with a Linux distribution (or any large software collection) comes the need to continuously test and update packages. An automation system (tentatively titled "Flux99") handles several tasks:
* Pulling together new changes
* Testing changes and identifying breakages
* Generating reports
* Uploading a nightly release

Since our goal is automation, we want the detection of breakages to be automated as well. Detecting breakages is an imperfect science: there are exponentially many combinations of different changes, and tests can be flaky. So in general we can only identify updates that have a high probability of causing a breakage. The problem falls under "stochastic scheduling", in particular determining which subset of changes to schedule a build for, given uncertain information about build successes/failures.

The general goal is to minimize the time/build resources needed for identifying breakages, i.e. to maximize the information gained from each build. Incremental building means that the most efficient strategy is often building in sequence, but this does not hold for larger projects where changes are almost independent.

Changes are discovered in an arbitrary order and similarly could be merged in any order. But an optimized order like "least likely to fail" could lead to arbitrarily long merge times for risky changes. It is simpler to do chronological order w.r.t. discovery. This could be customized to prioritize hotfixes before other changes, but it is easier to set up a dedicated code path for those.

To handle breakages, there are two main strategies: marking and backouts. Both are useful; a test failure may be unimportant or outdated, suggesting the marking strategy, while backouts reject bad changes from the mainline and keep it green. Backouts are harder to compute: for :math:`n` changes, there are :math:`2^n` possible combinations to test, giving a state space of size :math:`2^{2^n}`. Meanwhile marking only has :math:`2^n` states for :math:`n` commits. But marking is run over the entire commit history, hence has a huge commit list, while backouts are for pending changes and only need to consider the relevant set of new commits.

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

For backouts, we must first decide a backout strategy - given two sets of commits that both succeed, which set is preferred as the "green" mainline? The paper :cite:`ananthanarayananKeepingMasterGreen2019` provides a real-world case study. We should maximize the number of changes included and exclude later commits if the earlier ones succeed. So we prefer ``A,B`` to ``A,C`` because it has the earlier change ``B``. Similar we prefer ``A`` over ``B,C`` - to see why this makes sense, imagine ``A`` succeeds by itself and ``A,B,C`` is a failure - then to get ``B,C`` we would have to decide to test without ``A`` even though it succeeds. Since ``A`` could already been pushed to mainline this is unlikely to be the desired behavior. So the backout strategy is lexicographic preference: we write ``A,B`` and ``B,C`` as binary numbers ``110`` and ``011`` and compare them, and the higher is the chosen result.

Next we need a model predicting the success of a build. We assume that the build fails if it contains a failing configuration of certain commits left in or out. To avoid combinatorial explosion we assume that configurations are limited to 2 commits. This gives us 5 failing configurations:
* A left in - we say A is a breaking change
* A left out - we say A is a fixing change
* A left in, B left in - we say A and B conflict. Merge conflicts can often be detected immediately without running tests, but this also accounts for complex failures that arise from code interactions.
* A left in, B left out - we say A depends on B
* A left out, B left out - in this case both A and B fix the build. we say A and B are independent fixes

We use a probabilistic model to account for flakiness. Flakiness means that tests fail randomly even if everything ostensibly works and likewise can succeed even if something is broken.

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

The cost of compiling varies significantly based on the incremental state.

zipping is cheap. the testing fileset is smaller than the building fileset.


I compile each patch in the series one after another in the same directory, and after each compilation I zip up the files needed for testing. unzipping only needs to be done when bisecting is required.


Throwing patches out of the candidate requires recompiling all patches that were after the rejected patch. Adding patches requires an incremental compilation.



When testing a candidate, I run all tests without extending the candidate. I run the test that had not passed for the longest time, to increase confidence in more patches. If all the tests pass I update the state and create a new candidate containing all the new patches.
If any test fails I bisect to figure out who should be rejected, but don't reject until I've completed all tests. After identifying all failing tests, and the patch that caused each of them to fail, I throw those patches out of the candidate. I then rebuild with the revised candidate and run only those tests that failed last time around, trying to seek out tests where two patches in a candidate both broke them. I keep repeating with only the tests that failed last time, until no tests fail. Once there are no failing tests, I extend the candidate with all new patches, but do not update the state.

As a small tweak, if there are two patches in the queue from the same person, where one is a superset of the other, I ignore the subset. The idea is that if the base commit has an error I don't want to track it down twice, once to the first failing commit and then again to the second one.

If there is a failure when compiling, it caches that failure, and reports it to each step in the bisection, so Bake tracks down the correct root cause.