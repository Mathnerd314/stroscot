import subprocess
import sys
import difflib
import os
from src.compiler.evaluator import run_program

GOLDEN_DIR = os.path.join(os.path.dirname(__file__), "../../tests/golden")

def load_test_cases():
    """
    Load test programs and expected outputs. Returns list of (test_name, code, expected) tuples.
    For demo, hard-coded trivial programs and their expected output.
    """
    files = os.listdir(GOLDEN_DIR)
    test_cases = []
    for filename in files:
        if filename.endswith(".sct"):
            test_name = filename[:-4]
            with open(os.path.join(GOLDEN_DIR, filename), 'r') as f:
                code = f.read()
            with open(os.path.join(GOLDEN_DIR, filename + ".out"), 'r') as f:
                expected_output = f.read()
            test_cases.append((test_name, code, expected_output))
    return test_cases

def run_tests() -> None:
    tests = load_test_cases()
    passed = 0
    for test_name, code, expected in tests:
        output = run_program(code)
        if output == expected:
            print(f"[PASS] {test_name}")
            passed += 1
        else:
            print(f"[FAIL] {test_name}")
            # print("Expected Output:")
            # print(expected)
            # print("Actual Output:")
            # print(output)
            # print("Diff:")
            expected_lines = expected.splitlines(keepends=True)
            if expected and not expected.endswith('\n'):
                expected_lines.append("No newline at end of line\n")
            output_lines = output.splitlines(keepends=True)
            if output and not output.endswith('\n'):
                output_lines.append("No newline at end of line\n")
            diff = difflib.unified_diff(
                expected_lines, output_lines,
                fromfile='Expected Output', tofile='Actual Output', n=max(len(expected_lines), len(output_lines)))
            # filter out initial diff header lines
            lines = list(diff)[3:]
            # print out lines with fixed line endings
            for line in lines:
                fline = line.rstrip('\n')
                print(fline, end='\n')
    print(f"{passed} / {len(tests)} tests passed.")
