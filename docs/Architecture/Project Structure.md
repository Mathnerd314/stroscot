# Project Structure and Build System

```
compiler-project/
├── src/                  # Source code for compiler components
│   ├── __init__.py
│   ├── main.py           # CLI entry and REPL
│   ├── compiler/         # Compiler core modules (lexer, parser, etc.)
│   ├── tests/            # Test runner for test cases
│   └── tools/            # REPL, logger, utils
├── tests/            # Test cases
│   └── golden/       # Golden outputs for tests
├── Makefile          # Makefile for build and test targets
├── README.md
├── .gitignore
└── requirements.txt
```
