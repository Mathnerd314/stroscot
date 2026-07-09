# Contributing

Thank you for your interest in this project. Compilers can be intimidating, and this project is still working on making itself an exception.

## Before You Start

- The language design is frozen, but the core architectural notes are dense, personal, and written from a language theory perspective. It will likely take several hours to understand one document, and there are 30+ documents. Even language models struggle with them.
- The component breakdown in the issue tracker is the implementation-level specification, and it's complete with respect to the language design. What's still being figured out is *how* to implement that design, one component at a time, and sometimes the language design notes are very relevant and sometimes they're out of date compared to current research.
- We are in an active bootstrapping phase. Some components can't be started until earlier ones exist — check the issue tracker to see what's currently unblocked ("ready for work" tag).

## Finding a Task

1. Go to the [Ready for Work](https://github.com/Mathnerd314/stroscot/issues/views/3287) view and look for issues tagged `ready for work` and also `good first issue` or `help wanted`.
2. The compiler is structured as a network of self-contained, "black box" components that use specific algorithms and data structures to solve problems (e.g. "restructure the IR using DFA minimization" or "calculate this dataflow analysis"). Implementing the component itself typically doesn't require understanding the whole compiler - however, integrating it into the compiler does. You can do just the component or also integrate it into the compiler, it's up to you.
3. Comment on the issue to claim it before starting, so we don't duplicate work.

If nothing is tagged and available, find a ready-for-work issue and propose something small — but please talk to a maintainer before starting large or architectural work, since the design is still being discovered in places.

Also feel free to submit refactors, "cleanup" PRs, style nits, etc. The documentation is supposed to be inviting to people of all skill levels, so documenting items like how to get started, where to find key files, expectations, etc. are all contributions that will be accepted. Commit access is available for anyone with 2 or more accepted pull requests.

## Building and Setup

```bash
git clone https://github.com/yourname/yourproject.git
cd yourproject
pip install -r requirements.txt
pytest
```

If this doesn't work cleanly on a fresh clone, that's a bug — please open an issue.

## Pull Request Checklist

Before submitting a PR:

- [ ] Tests pass locally (`pytest`). Where possible, add tests or at least a demo.
- [ ] New code has some docstring explaining *how* it solves the problem or at least what it does
- [ ] If the issue references an academic paper or algorithm, link it in a comment near the relevant code

If you want to contribute without touching core algorithms, try to break things:
- Feed the compiler unusual or malformed input.
- If you find a bug, write a minimal failing `pytest` case that reproduces it and open a PR (a failing test alone is a valid, welcome contribution).


## Questions

Open a discussion or comment directly on the relevant issue. Response times may vary, but expect within a week.
