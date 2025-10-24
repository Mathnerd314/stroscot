Here's a cleaned-up description of your programming language's abstract syntax, "Stroscot," based on the provided documentation:

Stroscot's abstract syntax is designed to support a blend of functional, stateful, and verified programming paradigms, incorporating features such as set definitions, stateful programming with blocks and do-notation, and term rewriting.

## Core Language Constructs

### Program Structure
* **Program**: Represents the root of the abstract syntax tree, containing a sequence of declarations and statements.

### Expressions
* **Expression**: A foundational node type representing a value. Expressions are immutable and reducible to at least one value.
    * **Fields**:
        * `text_representation`: UTF-8 representation for human interaction.
        * `in_memory_representation`: A distinct, possibly more compact, in-memory representation.
    * **Relationships**: Can be a sub-node in various contexts (e.g., arguments in an application, conditions in a rewrite rule).
* **Symbol** (inherits from `Expression`): The simplest expression, a sequence of characters.
    * **Fields**:
        * `name`: Sequence of characters (Unicode word for ordinary, escaped string for extended).
        * `is_extended`: Boolean, indicates if it's an extended symbol using an escape sequence.
    * **Relationships**: An extended symbol matches a corresponding ordinary symbol.
* **Value** (inherits from `Expression`): An expression that has been evaluated or is in a normal form.
    * **Fields**:
        * `content`: Can be a `Variable` or a `HeadNormalForm`.
* **Literal**: Represents basic values (e.g., numbers, booleans, strings).
* **Variable**: An identifier referring to a value or a binding.
    * **Fields**: `name` (String).
* **Application (Curried)**: Represents function application, where functions take arguments one at a time.
    * **Fields**:
        * `function`: An `Expression`.
        * `argument`: An `Expression`.
    * **Relationships**: `App(App(App(f,t1),t2),tn)` represents `f t1 t2 tn`.
* **Abstraction (Lambda Calculus)**: Represents anonymous functions.
    * **Fields**:
        * `variable`: A `Variable`.
        * `body`: An `Expression`.
* **Let Binding**: Introduces local variable definitions.
    * **Fields**:
        * `variable`: A `Variable`.
        * `value`: An `Expression`.
        * `body`: An `Expression`.
* **HeadNormalForm**: A conventional value, can include a `Tuple`.
* **Tuple**:
    * **Fields**:
        * `elements`: List of `Value`s.

### Statements and Blocks
* **Statement**: Represents an action or a sequence of actions. Can be part of a `Block` node or function body.
    * **Simple Statement**: An expression used as a statement.
        * **Fields**: `expression` (`Expression`).
    * **Non-Returning Statement**: A sequence of statements where the preceding expression's value is discarded.
        * **Fields**:
            * `expression`: An `Expression`.
            * `next_statement`: A `Statement`.
    * **Returning Statement**: Binds the result of an expression to a pattern within a block, then continues with subsequent statements.
        * **Fields**:
            * `pattern`: A `Pattern`.
            * `expression`: An `Expression`.
            * `next_statement`: A `Statement`.
    * **Empty Block**: A no-op statement.
    * **Return Statement**: Explicitly returns a value from a block.
        * **Fields**: `expression` (`Expression`).
* **Block**: A collection of statements.
    * **Fields**: `statements` (List of `Statement`).

### Functional Logic Constructs
* **Program**:
    * **Fields**: `expression`: A closed expression `e` (where `freevars(e) = ∅`).
    * **Relationships**: Takes the first result of the expression; fails if the expression fails.
* **Equation**:
    * **Fields**:
        * `variable`: A `Value` (specifically a variable `v`).
        * `expression`: An `Expression` `e`.
* **ExistentialQuantifier**:
    * **Fields**:
        * `variables`: List of `Variable` names.
        * `expression`: An `Expression`.
* **Failure**: Yields no values.
* **Choice**: Yields multiple values (expression1 followed by expression2).
    * **Fields**:
        * `expression1`: An `Expression`.
        * `expression2`: An `Expression`.
* **OneOf**: If the expression fails, it fails; otherwise, returns the first value yielded by the expression.
    * **Fields**:
        * `expression`: An `Expression`.
* **AllOf**: Reifies choices as a tuple; `n` choices mean a tuple of length `n`.
    * **Fields**:
        * `expression`: An `Expression`.

## Modules and Encapsulation

* **Program Node**:
    * **Fields**: A list of `Module` nodes, global `Declaration` nodes.
    * **Relationships**: Contains multiple modules and top-level declarations.
* **Module Node**:
    * **Fields**:
        * `name` (string).
        * `entities` (map from `Identifier` to `Entity` or `NamePath`).
    * **Relationships**: Can be nested within other `Module` nodes.
* **NamePath Node**:
    * **Fields**: `identifiers` (list of `Identifier` nodes or symbols).
    * **Relationships**: Refers to an `Entity`.
* **Import Node**:
    * **Fields**:
        * `name_path` (`NamePath` node).
        * `as_name` (`Identifier` node, optional for renaming).
    * **Relationships**: Resolves a `NamePath` to shorten it to a bare `Identifier`.
* **Encapsulation Node**:
    * **Fields**: `definitions` (list of `Declaration` nodes).
    * **Relationships**: Can inhibit external access to its internal definitions.
* **Identifier Node**:
    * **Fields**: `name` (string).
    * **Relationships**: Used in `NamePath`, `VariableDeclaration`, `FunctionDeclaration`, `TypeReference`, etc.

## Macros and Metaprogramming

* **MacroDefinition Node**:
    * **Fields**:
        * `name` (`Identifier` node).
        * `parameters` (list of `Identifier` nodes).
        * `body` (`AST` node template).
    * **Relationships**: `body` is an AST node that will be transformed.
* **FexprCall Node**:
    * **Fields**:
        * `fexpr` (`Expression` node representing the fexpr).
        * `arguments` (list of `AST` nodes).
        * `environment` (`Environment` context).
    * **Relationships**: `fexpr` takes code ASTs and a lexical environment, and can evaluate AST fragments multiple times.
* **AST Node**: Represents a fragment of code. Can be manipulated by macros and fexprs.

## Memory Management

* **TypeDefinition Node**:
    * **Fields**:
        * `name` (`Identifier` node).
        * `is_indirect` (boolean).
        * `components` (list of `TypeReference` or `FieldDefinition` nodes).
    * **Relationships**: Indirect types may have components of their own type and mutable substructure.
* **ConstructorCall Node**:
    * **Fields**: `type_reference` (`TypeReference` node).
    * **Relationships**: Creates a distinct element of an indirect type.
* **MemoryModel Node**:
    * **Fields**:
        * `model_type` (enum: `Pointer`, `Reference`).
        * `size` (integer, for pointers).
    * **Relationships**: Determines how memory is accessed (e.g., integer-indexed array or associative array).

## Operational Primitives

* **InputOutputOperation Node**:
    * **Fields**:
        * `operation_type` (enum: `Read`, `Write`, `Control`).
        * `channel` (`Identifier` or `Expression` node).
        * `data` (`Expression` node for writes).
    * **Relationships**: Represents low-level I/O operations.
* **MemoryOperation Node**:
    * **Fields**:
        * `operation_type` (enum: `Read`, `Write`).
        * `address` (`Expression` node).
        * `value` (`Expression` node for writes).
    * **Relationships**: Represents memory access.
* **ForeignFunctionCall Node**:
    * **Fields**:
        * `function_name` (string).
        * `arguments` (list of `Expression` nodes).
    * **Relationships**: Calls functions external to the Stroscot runtime.
* **IntrinsicCall Node**: Represents a call to a low-level operation or assembly facility exposed as an intrinsic.
    * **Fields**:
        * `intrinsic_name`: Identifier for the intrinsic function.
        * `arguments`: List of `Expression`s.
    * **Relationships**: Can be used to interrogate and control physical resources. Calls compiler-specific optimized operations.
* **SystemCall Node**:
    * **Fields**:
        * `call_name` (string).
        * `arguments` (list of `Expression` nodes).
    * **Relationships**: Calls operating system functions.
* **AssemblyInstruction Node**:
    * **Fields**:
        * `instruction_mnemonic` (string).
        * `operands` (list of `Expression` nodes or literals).
    * **Relationships**: Represents direct assembly instructions, potentially hardware-specific.
* **ForeignCodeInterface**: Defines an interface to other programming languages, including assembly languages.
    * **Fields**:
        * `foreign_language_id`: Identifier for the foreign language (e.g., "C", "Assembly").
        * `program_elements`: List of `ProgramElementMapping` nodes.
* **ProgramElementMapping**: Identifies program elements referenced in both the source language and foreign code.
    * **Fields**:
        * `source_language_element`: Reference to the element in the current language's AST.
        * `foreign_code_element_name`: Name or identifier of the corresponding element in the foreign code.

## Control Flow and Dispatch

### Clauses and Patterns
* **Clause**: A pattern-body pair that checks a term against a pattern and transforms it to the body if it matches.
    * **Fields**:
        * `pattern`: A `Pattern` node.
        * `body`: An `Expression` or `Statement` node representing the transformation.
    * **Relationships**: A clause is evaluated by matching its `pattern` against a `term` and, if successful, executing its `body`.
* **Pattern** (abstract base type): Predicates that specify a set of join points for aspects, or structures for matching in clauses. They bind variables and can fail or succeed.
    * **Relationships**: Can be combined with `AND` and `OR` patterns. `Pattern` can be a `Term`.
    * **Concrete Pattern Types**:
        * `WildcardPattern`: Matches any value without binding a name.
        * `AtomMatchPattern`: Matches a specific atom.
            * **Fields**: `atom_value`.
        * `SymbolTreePattern`: Matches a symbol tree with a specific atom and checks length.
            * **Fields**: `symbol_atom`, `min_length`.
        * `AnySymbolTreePattern`: Matches any symbol tree besides a single atom.
        * `LiteralMatchPattern`: Matches an exact literal value or structure.
            * **Fields**: `literal_value`.
        * `ListStartEndPattern`: Matches a list starting and ending with specific values.
            * **Fields**: `start_value`, `end_value`.
        * `RecordMatchPattern`: Matches a record with specific field values and allows for additional fields.
            * **Fields**: `field_matches`: Map of field names to `Value`s.
        * `AndPattern`: Matches if both sub-patterns match.
            * **Fields**: `pattern1`, `pattern2`.
        * `OrPattern`: Matches if either sub-pattern matches.
            * **Fields**: `pattern1`, `pattern2`.
        * `DesugaredPattern`: For patterns that desugar into `let` expressions.
            * **Fields**: `inner_pattern`.
        * `TypeTagPattern`: Matches based on type.
            * **Fields**: `value_pattern`, `type_expression`.
        * `GuardPattern`: Matches if an arbitrary function applied to the term returns true.
            * **Fields**: `value_pattern`, `guard_function_expression`.
        * `ViewPattern`: Applies a function to the term and matches the result against a sub-pattern.
            * **Fields**: `view_function_expression`, `result_pattern`.

### Function Definitions and Calls
* **FunctionDefinition**:
    * **Fields**:
        * `name`: Identifier.
        * `parameters`: List of `Parameter` nodes.
        * `body`: An `Expression` or `BlockStatement`.
* **FunctionCall**:
    * **Fields**:
        * `function_expression`: An `Expression` representing the function being called.
        * `arguments`: List of `Argument` nodes.
* **Argument**:
    * **Fields**:
        * `expression`: The `Expression` passed as an argument.
        * `is_partial_application_placeholder`: Boolean, `True` if it's an underscore `_` for partial application.
    * **Relationships**: Supports currying and partial application.

## Concurrency and Parallelism

* **ProcessDefinition**: Defines a parallel process.
    * **Fields**:
        * `name`: Identifier for the process.
        * `body`: The code block or expression defining the process's execution.
* **ProcessInitiation**: Initiates an instance of a defined process.
    * **Fields**:
        * `process_name`: Reference to a `ProcessDefinition` by its name.
        * `scope`: The lexical scope within which the process is initiated.
* **ProcessTermination**: Represents the termination of a process.
    * **Fields**:
        * `process_name`: Reference to a `ProcessDefinition` by its name.
* **PriorityAssignment**: Allows a process to alter its own priority.
    * **Fields**:
        * `priority_value`: An integer or enum representing the new priority.
* **ParallelizationDirective**: Marks pure computations for automatic parallelization.
    * **Fields**:
        * `computation`: The `Expression` or `BlockStatement` to be parallelized.
        * `strategy_parameter`: Optional, parameter for task queue implementation (e.g., work-stealing).

## Exception Handling

* **ExceptionHandlingBlock**: A mechanism for responding to unplanned error situations.
    * **Fields**:
        * `try_block`: A `BlockStatement` containing code that might throw exceptions.
        * `catch_clauses`: List of `CatchClause` nodes.
        * `finally_block`: Optional `BlockStatement` for cleanup code.
* **CatchClause**:
    * **Fields**:
        * `exception_type`: Type or pattern matching the exception to be caught.
        * `exception_variable`: Optional, a variable to bind the caught exception.
        * `handler_block`: A `BlockStatement` containing the exception handling logic.
* **ThrowStatement**: Explicitly raises an exception.
    * **Fields**:
        * `exception_expression`: An `Expression` representing the exception value to be thrown.
* **Assertion**: A program-specified assertion that can detect errors during execution if not satisfied.
    * **Fields**:
        * `condition`: A Boolean `Expression` that must evaluate to true.
    * **Relationships**:
        * `AssertStatement`: Errors if the condition is false at runtime, or if the compiler can't prove it true.
        * `AssumeStatement`: Prunes execution traces where the expression is false.
* **ExceptionType** (abstract base type):
    * **Concrete Exception Types**:
        * `FailureException`: Generic explicit throw (e.g., `undefined`, `error "message"`).
        * `DomainException`: An alternate result when a function's precondition is false (e.g., `DivideByZero`, `IntegerOverflow`, `IndexOutOfBounds`).
        * `SystemException`: Failures due to external state changes (e.g., `FileNotFound`, `DatabaseCorrupted`, `NetworkUnreachable`).
        * `HardwareException`: Low-level exceptions triggered by hardware (e.g., `SIGBUS`, `SIGSEGV`).

## Aspects and Cross-Cutting Concerns

* **Aspect**: A programming paradigm for separating concerns that cut across multiple modules.
    * **Fields**:
        * `pointcut`: A `Pointcut` node.
        * `advice`: An `Advice` node.
        * `stateful_variables`: Optional, shared state among aspect invocations.
* **Pointcut**: A predicate specifying a set of `JoinPoint`s.
    * **Fields**:
        * `predicate_expression`: A logical `Expression` that evaluates to true for a given `JoinPoint`.
        * `scope`: Optional, limitations like a class, method, or module.
* **Advice**: Code executed at chosen `JoinPoint`s, transforming the `JoinPoint` action.
    * **Fields**:
        * `code_block`: A `BlockStatement` or `Expression` to be executed.
        * `type`: (e.g., `before`, `after`, `around`).
* **JoinPoint**: A specific point during application execution (e.g., before/after a method call, field access, object instantiation).
    * **Fields**:
        * `type`: (e.g., `MethodCall`, `FieldAccess`, `ObjectInstantiation`).
        * `location`: Reference to the specific code location.
* **Introduction**: Introducing new methods, fields, or interfaces into a class at runtime.
    * **Fields**:
        * `target_class`: Reference to the class being modified.
        * `new_members`: List of `MethodDefinition`, `FieldDefinition`, or `InterfaceImplementation` nodes.

## Logic Programming

* **PredicateDefinition Node**:
    * **Fields**:
        * `name` (`Identifier` node).
        * `clauses` (list of `Clause` nodes).
    * **Relationships**: Defines a logical relation.
* **Clause Node**:
    * **Fields**:
        * `head` (`PredicateCall` node).
        * `body` (list of `PredicateCall` nodes or `Literal` nodes).
    * **Relationships**: Represents a Horn clause (e.g., `Head :- Body`).
* **PredicateCall Node**:
    * **Fields**:
        * `predicate_name` (`Identifier` node).
        * `arguments` (list of `Term` nodes).
    * **Relationships**: Calls a predicate, used in clause heads and bodies.
* **Term Node** (Base Type):
    * **Relationships**: Can be a `Variable`, `Constant`, or `Structure`.
* **Variable Node**:
    * **Fields**: `name` (`Identifier` node).
* **Constant Node**:
    * **Fields**: `value` (literal value).
* **Structure Node**:
    * **Fields**:
        * `functor` (`Identifier` node).
        * `arguments` (list of `Term` nodes).

## Object-Oriented Programming Concepts

* **Object Node**:
    * **Fields**:
        * `properties` (map from `Identifier` to `Expression` nodes).
        * `methods` (map from `Identifier` to `FunctionDeclaration` nodes).
    * **Relationships**: Groups related data and behavior.
* **PropertyAccess Node**:
    * **Fields**:
        * `object` (`Expression` node).
        * `property_name` (`Identifier` node).
    * **Relationships**: Accesses a property of an object.
* **MethodCall Node**:
    * **Fields**:
        * `object` (`Expression` node).
        * `method_name` (`Identifier` node).
        * `arguments` (list of `Expression` nodes).
    * **Relationships**: Invokes a method on an object.
* **Inheritance Node**:
    * **Fields**:
        * `child_type` (`TypeReference` node).
        * `parent_type` (`TypeReference` node).
    * **Relationships**: Represents the mechanism for types to inherit properties and methods.
* **Polymorphism Node**: Not a concrete node itself, but rather a property of `FunctionDeclaration` or `MethodDeclaration` nodes, indicating support for different behaviors based on input types or runtime dispatch.

## Posets (Partial Orders)

* **PartialComparisonOperator Node**:
    * **Fields**:
        * `left_operand` (`Expression` node).
        * `right_operand` (`Expression` node).
    * **Relationships**: Returns one of `LessThan`, `Equal`, `GreaterThan`, `Incomparable`. Used for defining operator precedence and method combination.

## Resource Management

* **DestructorDefinition Node (or Finalizer)**:
    * **Fields**: `cleanup_action` (`Statement` node or `Block` node).
    * **Relationships**: Associated with a resource acquisition to ensure prompt release.
* **ResourceAcquisition Node**:
    * **Fields**:
        * `resource_type` (e.g., `FileHandle`, `Mutex`).
        * `acquisition_expression` (`Expression` node).
        * `destructor_reference` (`DestructorDefinition` or `Identifier` for named destructor).
    * **Relationships**: Acquires a resource and links it to a cleanup mechanism.
* **TryBlock Node**:
    * **Fields**:
        * `protected_statements` (`Block` node).
        * `finally_statements` (`Block` node, optional).
        * `catch_clauses` (list of `CatchClause` nodes, if exceptions are supported).
    * **Relationships**: Ensures cleanup actions (in `finally_statements`) are executed regardless of early exit.
* **UsingStatement Node (or `try-with-resources` / `with` equivalent)**:
    * **Fields**:
        * `resource_binding` (`VariableDeclaration` node).
        * `body` (`Block` node).
    * **Relationships**: Binds an acquired resource to a variable and ensures its release when the block exits.

## Set and Type System Constructs

* **Set Definition**:
    * **Fields**:
        * `symbol` (Symbol/Identifier).
        * `predicate` (Expression - a boolean function).
    * **Relationships**: Can be defined by overloading `isElementOf` for a symbol.
* **Set Literal**: A direct enumeration of values in a set.
    * **Fields**: `elements` (List of `Value`/`Expression`).
* **Set Builder Notation**: Defines a set based on a predicate and an existing set.
    * **Fields**:
        * `variable` (`Variable`).
        * `base_set` (`Set`).
        * `predicate` (`Expression`).
* **Set Operation**:
    * **Types**:
        * **Union**: `left_set` (`Set`), `right_set` (`Set`).
        * **Intersection**: `left_set` (`Set`), `right_set` (`Set`).
        * **Complement**: `operand_set` (`Set`).
        * **Difference**: `left_set` (`Set`), `right_set` (`Set`).
* **Membership Test**: `isElementOf` predicate.
    * **Fields**:
        * `value` (`Expression`).
        * `set` (`Set`).
    * **Returns**: Boolean.
* **Type Annotation/Assertion**: Declaring that a value is a member of a set.
    * **Fields**:
        * `expression` (`Expression`).
        * `set` (`Set`).
    * **Example Syntax**: `a : B`.

## Term Rewriting Constructs

* **Rewrite Rule**: The fundamental unit of term rewriting.
    * **Types**:
        * **Unconditional Rewrite Rule**:
            * **Fields**: `left_hand_side` (Term/Pattern), `right_hand_side` (Term/Expression).
        * **Conditional Rewrite Rule**:
            * **Fields**: `left_hand_side` (Term/Pattern), `condition` (Predicate/Expression), `right_hand_side` (Term/Expression).
* **Match Expression**: Allows pattern matching against a value.
    * **Fields**:
        * `value` (`Expression`).
        * `cases` (List of `Match Case`).
* **Match Case**: A single branch within a match expression.
    * **Fields**:
        * `pattern` (`Pattern`).
        * `body` (`Expression`).
    * **Special Cases**: `otherwise` for a default case.

## Monad Comprehension Constructs

* **Monad Comprehension**:
    * **Fields**:
        * `result_expression` (`Expression`).
        * `qualifiers` (List of `Qualifier`).
* **Qualifier**:
    * **Types**:
        * **Generator**: Binds a pattern to the result of an expression.
            * **Fields**: `pattern` (`Pattern`), `expression` (`Expression`).
        * **Filter/Guard**: A boolean expression that filters results.
            * **Fields**: `condition` (`Expression`).
        * **Let Binding**: Introduces local definitions within the comprehension.
            * **Fields**: `declaration` (`Declaration`).
        * **Parallel Comprehension**: Combines results from multiple independent qualifiers.
            * **Fields**: `left_qualifier` (`Qualifier`), `right_qualifier` (`Qualifier`).
        * **Transform Comprehension**: Applies a function to the generated values.
            * **Fields**: `qualifier` (`Qualifier`), `transform_function` (Function/Expression).
            * **Optional Fields**: `group_by_expression` (`Expression`) for `then group by` clauses.
* **Guard Function Call**: Special function for filtering in monad comprehensions.
    * **Fields**: `condition` (Bool Expression).
