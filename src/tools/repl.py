from src.compiler.evaluator import run_program

def repl() -> None:
    print("Simple REPL. Type 'exit' to quit.")
    while True:
        user_input = input(">>> ")
        if user_input.strip() == "exit":
            print("Goodbye!")
            break
        output = run_program(user_input)
        print(f"Output:\n{output}")
