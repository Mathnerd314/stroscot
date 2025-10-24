Stroscot is a programming language designed to combine different ways of writing code: functional (like math equations), stateful (where programs remember things), and verified (where programs are proven correct). It includes features for defining sets, managing program state using blocks, and controlling code using term rewriting.

### How Programs are Built and What They Use

A Stroscot **Program** is like the main blueprint for your code, containing all the instructions and definitions. At its core are **Expressions**, which are pieces of code that produce a value. The **program entry point** itself is a special expression that represents the entire codebase. Stroscot will run your program by evaluating this **main expression**. In the simplest case, this is just a an integer which becomes the program's return code, but Stroscot includes full support for imperative programming, meaning you can interact with the computer's memory and perform actions that change the program's state.

### Organizing Code: Modules

A `Program` can contain multiple **Module** nodes and global declarations. Each **Module** has a name and contains its own definitions. Modules can even be placed inside other modules. **NamePath Nodes** help you refer to things using a clear path of names. **Import Nodes** let you shorten these paths so you don't have to type them out fully every time. **Encapsulation Nodes** group definitions and can restrict what other parts of the code can access inside them. **Identifier Nodes** are just names used for various parts of the code.

### Expressions

An **Expression** is a piece of code that can be evaluated. A **Symbol** is an expression that is just a name made of characters. **Literals** are basic values like numbers, true/false, or text. Stroscot also supports **term rewriting**, in particular a **Term** which is a symbol applied to multiple expressions. There is also a **Tuple**, a collection of values.

An **Application** means applying a function to its arguments, and it is curried, so one at a time. The similar **FunctionCall** node represents when you use or "call" a function, providing the function and its inputs (arguments), but not curried. **Argument** nodes hold the values you pass to a function and can also indicate that you're only partially providing arguments (for "currying").  Lambda Calculus **Abstraction** creates unnamed functions with a variable and a body of code.

**Variables** are expressions consisting of names that refer to values or other definitions. A **Let Binding** lets you define local variables that only exist within a specific part of your code. Variables can also be defined by the module system or pattern matching.

A **Set Definition** defines a set; it can either be a **Set Literal** which is a direct list of values in a set, or **Set Builder Notation** which defines a set based on a logical rule. **Set Operation** nodes let you perform standard set operations like union (combining sets), intersection (finding common elements), complement, and difference. **Membership Test** checks if a value is part of a set. **Type Annotation/Assertion** declares that a value belongs to a specific set (which can act as a type). Special **Set Declaration** nodes define module-level sets in an extensible way across the program. **Exceptions** are defined via a special set and handling functions, allowing you to define and handle errors in a structured way.

An **Object** is a special type of record which groups together data (properties) and actions (methods). **PropertyAccess Nodes** let you get to the data within an object. **MethodCall Nodes** let you tell an object to perform one of its actions. The inheritance built-in function makes objects with the properties and methods from another object.

A **Monad Comprehension** is a way to build up results by combining an expression with "qualifiers". A **Monad Comprehension** translates to a series of **Monad** operations strung together, allowing you to build complex data flows in a structured way. **Qualifier** types include:
* **Generator**: Takes a value from an expression and binds it to a pattern. **Guard Function Call** is a specific function used for filtering in these comprehensions.
* **Filter/Guard**: A true/false expression that filters results.
* **Let Binding**: Defines local variables within the comprehension.
* **Parallel Comprehension**: Combines results from multiple independent qualifiers.
* **Transform Comprehension**: Applies a function to the generated values, with an optional `group_by_expression` for grouping results.

### Actions and Code Blocks

**Statements** are like instructions that tell the program to do something. They can be part of a **Block**, which is just a group of statements. A **Simple Statement** is an expression used as an instruction. A **Non-Returning Statement** performs an action but doesn't produce a value you use later. A **Returning Statement** performs an action and then gives you back a value, which you can assign to a "pattern". An **Empty Block** does nothing. A **Return Statement** specifically sends a value back from a block.

### Logical Programming Features

Within Set Builder notation, you can use various built-in functions and operators, such as **Exists**, **ForAll**, **Not**, **And**, **Or**, **Equal**, etc. The special functions **Failure**, **Choice**, **OneOf**, and **AllOf** translate between logic programming and Stroscot's representation of a logic programs as a set.

### Controlling Program Flow: Patterns, Functions, and Macros

Stroscot uses **Patterns** to control how code runs based on the data it receives. Patterns are used in **Clause** nodes, which are part of a **FunctionDefinition** or **MacroDefinition**.

A **Rewrite Rule** or **Clause** node is a pair of a "pattern" and a "body" of code. If a piece of data matches the pattern, the code in the body is executed. A **Pattern** is like a template that defines a structure to match against. There are many specific types of patterns, such as `WildcardPattern` (matches anything), `AtomMatchPattern` (matches a specific basic value), `ListStartEndPattern` (matches lists that start and end with certain values), and `AndPattern` and `OrPattern` (combining other patterns). There's also an `otherwise` case for a default action.

A **FunctionDefinition** node runs clauses using the standard "applicative" logic; arguments are evaluated before the function is called and the matching clause is executed. Clauses are matched in order, and the first one that fits the input data is executed. A **FunctionCall** node is used to call a function with its arguments.

A **MacroDefinition Nodes** define "macros," which are clauses that manipulate code as abstract syntax. An **AST Node** represents a piece of code wrapped so that it isn't evaluated. Macros operate on these AST Nodes, allowing you to transform code before it runs, and then run the transformed code with the special **Evaluate** node.

### Memory, Resources, and Concurrency

Pointers and references are used to manage memory in Stroscot. They are manipulated through special functions to allocate, get, and set values in memory. Destructors are a special function defining actions to clean up resources, ensuring they are released properly. Stroscot uses destructors in the RAII style so cleanup actions are always run. Special functions for spawning and managering threads and processes are also available, allowing you to run code in parallel, but they don't have any special syntax in the language itself.

### Low-Level Operations

Stroscot has special functions to interact with the computer's core functions: reading from and writing to external devices, direct reads and writes to memory addresses, calling functions written in other programming languages, calling low-level intrinsic, direct assembly code or machine instructions, operating system function, and executing code written in other languages and marshalling data to/from its foreign representation.
