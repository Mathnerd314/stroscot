# The Stroscot Programming Language

Stroscot is "an imperative programming language designed for modern processors". More specifically: an experimental, general-purpose systems language and compiler architecture, built by synthesizing decades of programming language research into a single, coherent architecture.

## Status: Implementation Phase (Design Frozen)

The theoretical design of the language is finalized, based on a broad literature review spanning existing programming languages and academic research. The compiler is not yet self-hosting — it currently deals with a minimal subset of the language in a Python prototype. Getting to self-hosting is the current milestone.

## Roadmap

See https://github.com/Mathnerd314/stroscot/issues/40 for the live, up-to-date status of every component. Although presented as a flat list, the components depend and interact with each other, but they are in implementation order.

## What Is This, Really?

Where other languages (Rust included) intentionally stuck to conservative, production-proven research, this project takes a wider, more speculative slice of the literature — including prototypes and theses that were never hardened for production. This is a research compiler. Some of the ideas we're implementing may not survive contact with a real system. That's the point.

The prototype is written in Python, and the language is designed to be surface-level Python-compatible. This isn't a performance gimmick — it means the bootstrap compiler can eventually compile itself with minimal rewriting, and it means you can start contributing to a systems-level compiler today, in a language you probably already know.

## What You'll Learn

Contributing here means hands-on exposure to:
- Advanced academic compiler theory
- Low-level operating system design
- NP-hard optimization and verification problems
- Software project component composition models and architecture

See [CONTRIBUTING.md](./CONTRIBUTING.md) for how to find a task, how we use tests as documentation, and what we expect in a pull request.

## Layout

* docs - main design notes, generated with Sphinx. Live at https://mathnerd314.github.io/stroscot/ warning: dense and written for personal use
* extern - external projects that may prove useful for the build system
* glue - random notes/pseudocode on various aspects
* library - sketches of some standard library aspects
* src - optimal reduction (core) prototype and imports of some external projects
* tests - smaller Haskell experiments and test cases
* new-sprint - Python tree where most development is happening now

## License

The target for self-hosting is Apache 2.0 but we have picked up some bootstrapping dependencies (iced - MIT, Valgrind VEX - GPL) so be careful.
