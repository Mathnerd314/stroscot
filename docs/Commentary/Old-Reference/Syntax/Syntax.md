# Syntax

Stroscot uses a unique, LLM-centric approach to processing source code. Instead of traditional compilers that rely on rigidly defined lexers and parsers, Stroscot leverages the natural language understanding and generation capabilities of Large Language Models to transform user-written code into an executable internal representation (IR). This process is divided into two distinct phases: a "Clean-Up Phase" for standardization and a "Parsing Phase" for generating the IR.

## Unicode

All text in Stroscot is required to be encoded in [UTF-8](https://en.wikipedia.org/wiki/UTF-8). This includes source code, comments, and any other text-based elements. UTF-8 is a variable-width character encoding that can represent every character in the Unicode character set, making it suitable for a wide range of languages and symbols.

Stroscot uses Unicode's [Normalization Form C (NFC)](https://unicode.org/reports/tr15/#Norm_Forms) to ensure that characters are represented in a consistent way. NFC normalization is important because it ensures that characters that can be represented in multiple ways (e.g., composed and decomposed forms) are standardized to a single form, which helps avoid issues with string comparison and processing.

Stroscot's source code is processed as a sequence of bytes, which are then interpreted as UTF-8 encoded characters. This means that any text input, including identifiers, keywords, and comments, must be valid UTF-8. If invalid byte sequences are encountered, they are replaced with Unicode's REPLACEMENT CHARACTER (U+FFFD) to maintain the integrity of the text processing. Stroscot then [NFC](http://unicode.org/reports/tr15/#Norm_Forms) normalizes the input, warning if input isn't already normalized.

This approach allows Stroscot to handle a wide variety of characters and symbols, making it flexible for internationalization and diverse programming styles.

Stroscot does not perform any case transformations on identifiers. It is case-sensitive, meaning that identifiers with different cases (e.g., `Variable`, `variable`, and `VARIABLE`) are treated as distinct entities. This allows for greater flexibility in naming conventions and avoids potential conflicts that can arise from case-insensitive handling.

## Phase 1: The Clean-Up Phase (Lexical Analysis)

The initial step in processing any code written in Stroscot is to put it through a **Clean-Up Phase**. The primary goal of this phase is to ensure code consistency and readability, effectively acting as Stroscot's lexical analysis. This phase addresses the inherent variability in human coding styles by normalizing the input code into a consistent, "fixed-point" format.

### How it Works

1.  **Initial Application of the Clean-Up Prompt:** When a user submits their code, a specialized "clean-up" prompt is applied to the LLM. This prompt instructs the LLM to standardize various aspects of the code, including:

      * **Whitespace:** Consistent indentation, spacing around operators, and blank lines.
      * **Naming Conventions:** Adherence to predefined conventions for variables, functions, and other identifiers.
      * **Commenting Style:** Uniformity in how comments are written and placed.
      * **Keyword Usage:** Consistent capitalization or formatting of keywords.
      * **Operator Spacing:** Standardized spacing around arithmetic, logical, and assignment operators.
      * **Bracket/Parentheses Placement:** Consistent style for opening and closing brackets, parentheses, and braces.
      * **Phrasing and Terminology:** Ensuring that the language used in comments and documentation is clear, concise, and follows a consistent style.
      * **Line Wrapping:** Removing line wrapping, as the Parsing Phase does not support line wrapping. An additional pass may be used to re-introduce line wrapping in the on-disk file, if users wish to ensure that lines do not exceed a certain length for readability.

2.  **Iterative Refinement to a Fixed Point:** Because LLMs can sometimes introduce subtle variations or fail to fully address all style issues in a single pass, the clean-up prompt is applied **repeatedly** to the code. After each application, the output is compared to the input. This iterative process continues until a "fixed point" is reached, meaning that applying the clean-up prompt no longer results in any changes to the code. This ensures a thorough and stable clean-up. If the code does not reach a fixed point after a reasonable number of iterations, it is considered invalid and an error is raised.

3.  **Defining "Tokens":** In a traditional compiler, lexical analysis breaks down source code into a stream of tokens (keywords, identifiers, operators, literals, comments, whitespace). In Stroscot, the fixed-point output of the Clean-Up Phase implicitly defines these "tokens." The consistent formatting makes it easier for the subsequent Parsing Phase to identify and interpret these fundamental building blocks of Stroscot. This standardized output is crucial for the LLM to reliably recognize and interpret the structural elements of Stroscot code.

### Importance of the Clean-Up Phase

This phase is critical for several reasons:

  * **Consistency:** It guarantees that all code, regardless of the original author's style, adheres to a single, readable format.
  * **Readability:** Standardized code is easier for developers to read, understand, and maintain, fostering collaboration.
  * **LLM Robustness:** By providing a consistent input to the Parsing Phase, it significantly increases the reliability and accuracy of the LLM's parsing capabilities. The LLM won't be distracted by stylistic variations and can focus solely on the structural and semantic aspects of the code.

#### Syntax highlighting

Syntax highlighting provides visual cues in code editors by coloring different elements of the code (keywords, variables, strings, etc.) to enhance readability and comprehension. The scientific literature on syntax highlighting generally supports that consistent syntax highlighting can improve code legibility and comprehension, particularly for novice programmers. The specific mechanism by which syntax highlighting aids in code comprehension is that a programmer is able to quickly scan through the code for a specific color or style, corresponding to a specific type of token, whereas without syntax highlighting, the programmer would have to read through the code more carefully to identify the same elements. For example, it is much easier to search for the color red than to count parentheses. This is also useful in large codebases or complex code structures. Programmers also tend to find syntax-highlighted code more visually appealing, which can contribute to a more comfortable coding experience. However, the effectiveness of syntax highlighting can vary based on several factors, including the programmer's experience level, whether the task is scanning-intensive, and the design of the highlighting scheme.

For Stroscot, the Clean-Up Phase ensures that the code is consistently formatted, and one such goal of the Clean-Up Phase is to produce a standardized output that is inherently readable, including the necessary structure and formatting for traditional syntax highlighting.

## Phase 2: The Parsing Phase (Internal Representation Generation)

Once the code has been cleaned up and reached its fixed-point state, it proceeds to the **Parsing Phase**. In this phase, a specialized "parsing" prompt is applied to the LLM, instructing it to transform the cleaned-up source code into Stroscot's Internal Representation (IR). This IR serves as a detailed syntax tree that directly maps to the internal code structure.

### Internal Representation (IR) Design Principles

The design of Stroscot's IR is a key differentiator, as it's specifically tailored to be "natural" for an LLM to generate. Stroscot's IR aims to combine features from several programming paradigms:

  * **Syntax (Python, Julia, Lua):** The general structure and readability of the IR will draw inspiration from these languages, known for their clear and often concise syntax. This suggests a block-based structure, clear function definitions, and perhaps emphasis on indentation for scope.
  * **Functional Features (Haskell, Rust):** Concepts like immutability, higher-order functions, pattern matching, and possibly type annotations will be present in the IR, reflecting the functional aspects of Stroscot.
  * **Logic Programming Features (Prolog):** The IR will need to represent logical predicates and unification. This might manifest as specific IR nodes for rules, facts, and queries.
  * **Object-Oriented/Concurrent Features (JavaScript, Java):** Features like objects, classes (or similar constructs), and concurrency mechanisms will also be expressible within the IR. This might involve nodes for method calls, object instantiation, or asynchronous operations.

### The "Natural" IR and Parsing Prompt

The core insight here is that the IR and the parsing prompt are designed synergistically, allowing the LLM to output what it is most comfortable with. This means:

1.  **LLM-Centric Structure:** Instead of forcing the LLM to output a rigid format like JSON (which can be challenging for models not specifically trained on it), Stroscot designs its IR's textual representation to align with the LLM's "natural" output style, which is often closer to Markdown or code-like structures. This might involve:

      * **Indentation-based hierarchy:** Similar to Python, where indentation defines scope.
      * **Keyword-driven constructs:** Using distinct keywords for different IR node types (e.g., `def_function`, `match_case`, `if_statement`).
      * **Structured comments or annotations:** The IR itself might contain embedded comments or specific annotation syntax that the LLM finds easy to generate for metadata.

2.  **Detailed Syntax Grammar:** The parsing prompt will instruct the LLM to generate code, and the output will be constrained to follow a formal context-free grammar, creating code that is both human-readable and machine-executable. This grammar will define the structure of the IR, ensuring that the LLM produces a consistent and valid representation. The grammar will be designed to be "natural" and "intuitive" for the LLM, in the sense that the LLM in combination with the prompt is highly likely to output code following the CFG even in the absence of the CFG constraint. Experiments will have to be conducted to find the right balance between verbosity and simplicity, for example, whether the LLM is more inclined to generate `if x > 5: return true` or a more structured and easy to parse representation like:
    ```
    IR_NODE: IfStatement
        CONDITION: BinaryExpression
            OPERATOR: GreaterThan
            LEFT: Identifier "x"
            RIGHT: Literal 5
        BODY: Block
            IR_NODE: ReturnStatement
                VALUE: Literal_Boolean true
    ```
    It seems clear that the LLM can be prompted to generate either, but the goal is to find a representation that the LLM can produce naturally for even complex natural-language input.

### Importance of the Parsing Phase

This phase is paramount for transforming human-readable code into a form that Stroscot's runtime or compiler can understand and execute.

  * **Semantic Understanding:** The parsing prompt guides the LLM to not just reproduce the code, but to understand its underlying structure and semantics, translating it into a formal representation.
  * **Foundation for Execution:** The generated IR is the direct input for subsequent stages of Stroscot's pipeline, such as interpretation, compilation, or static analysis.
  * **Flexibility:** By leveraging an LLM for parsing, Stroscot gains a unique flexibility in handling variations in input syntax, as long as the LLM can interpret them correctly after the Clean-Up Phase. This can lead to a more permissive and user-friendly initial coding experience.

## Phase 3: Classical Parse into Explicit Node Types and Fields

After this LLM processing, the CFG-conforming text will be parsed with classical techniques and structured with explicit node types and fields, creating a detailed syntax tree that maps closely to the internal code representation. This structure will be designed to be "natural" for the LLM, meaning it should be a format that the LLM can easily produce without excessive complexity. The goal is to create a representation that is both human-readable and machine-friendly, ensuring that the LLM can generate it without confusion or ambiguity.

A recursive-descent parser is a viable option for simpler grammars. The language emitted by the LLM in the Parsing Phase is expected to be LL(1) or LL(k), which means it can be parsed using a recursive-descent parser. This approach allows for a straightforward mapping of grammar rules to functions, where each function corresponds to a non-terminal in the grammar. The recursive descent parser will handle the parsing of the CFG-conforming text generated by the LLM, creating a structured representation that can be further processed or executed. Having an LLM generate the initial recursive-descent parser code is an interesting and potentially rapid prototyping strategy. The LLM could translate each EBNF rule into a corresponding function, handling the basic parsing logic.

ANTLR is another robust alternative. It can handle complex grammars, including those with operator precedence and associativity, which are crucial for expressions. ANTLR's generated parsers typically have robust error reporting and recovery mechanisms, which are difficult to implement correctly in a hand-written recursive-descent parser. ANTLR supports generating parsers in multiple languages (Java, C#, C++, Python, JavaScript, Go, Swift), offering flexibility. A careful, rule-by-rule analysis of the EBNF grammar is already needed for the transformation of the EBNF into `llama.cpp`'s GBNF format, so defining the ANTLR grammar would be a relatively small additional step. Maintaining a consistent EBNF source that can be easily transformed into both ANTLR and GBNF formats will reduce overhead and potential for errors.
