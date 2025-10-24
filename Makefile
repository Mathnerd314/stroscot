.PHONY: test build clean repl

build:
    @echo "Build step (add compilation steps here)"

test:
    python3 -m src.tests.runner

clean:
    @echo "Clean build artifacts (if any)"

repl:
    python3 src/main.py --repl
