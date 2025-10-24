import argparse
import logging
from src.tools.repl import repl
from src.tests.runner import run_tests

def main() -> None:
    parser = argparse.ArgumentParser(description="Compiler Project CLI")
    parser.add_argument('--test', action='store_true', help="Run test suite")
    parser.add_argument('--repl', action='store_true', help="Start REPL")
    args = parser.parse_args()

    logging.basicConfig(level=logging.DEBUG, format='[%(levelname)s] %(message)s')

    if args.test:
        run_tests()
    elif args.repl:
        repl()
    else:
        logging.info("No command specified. Use --test or --repl.")

if __name__ == "__main__":
    main()
