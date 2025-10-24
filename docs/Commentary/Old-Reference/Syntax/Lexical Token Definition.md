# Lexical Token Definition

This document formally defines the token types for Stroscot, intended for use in a TextMate grammar for syntax highlighting. The definitions closely follow TextMate's scope naming conventions to ensure broad compatibility with existing themes. Regular expressions match typical patterns, but may need refinement.

### 1. Shebang
-   **Description**: Interpreter directive at the very beginning of a script.
-   **Scope Name**: `comment.line.number-sign.shebang`
-   **Rule**: Must occur on the first line.
-   **Regex Example**: `^#!.*`
### 2. Keywords/Reserved Words
This category encompasses the core vocabulary of the language, influencing control flow, definitions, and other semantic aspects.
-   **Scope Prefix**: `keyword`
#### 2.1. Control Flow
-   **Description**: Keywords that govern the execution flow of the program.
-   **Scope Name**: `keyword.control.{keyword_name}` (e.g., `keyword.control.if`)
-   **Keywords**: `if`, `else`, `elseif`, `for`, `while`, `do`, `until`, `repeat`, `break`, `continue`, `return`, `yield`, `goto`, `pass`, `then`, `switch`, `case`
-   **Regex Example**: `\b(if|else|elseif|for|while|do|until|repeat|break|continue|return|yield|goto|pass|then|switch|case)\b`
#### 2.2. Definitions, Declarations, and Assignments
-   **Description**: Keywords used for defining functions, classes, variables, and type declarations.
-   **Scope Name**: `storage.type.{keyword_name}` (e.g., `storage.type.def`, `storage.type.class`)
-   **Keywords**: `def`, `function`, `class`, `struct`, `enum`, `interface`, `let`, `var`, `const`, `local`, `nonlocal`, `global`, `type`
-   **Regex Example**: `\b(def|function|class|struct|enum|interface|let|var|const|local|nonlocal|global|type)\b`
#### 2.3. Headers, Modules, and Imports
-   **Description**: Keywords related to module, package, and import management.
-   **Scope Name**: `keyword.control.import.{keyword_name}` (e.g., `keyword.control.import.module`)
-   **Keywords**: `module`, `package`, `import`, `export`, `use`, `from`, `as`, `program`, `source_file`
-   **Regex Example**: `\b(module|package|import|export|use|from|as|program|source_file)\b`
#### 2.4. Operators & Fixity Declarations
-   **Description**: Keywords used for declaring operator precedence or associativity.
-   **Scope Name**: `keyword.operator.declaration.{keyword_name}` (e.g., `keyword.operator.declaration.infix`)
-   **Keywords**: `infix`, `infixl`, `infixr`, `fixity`
-   **Regex Example**: `\b(infix|infixl|infixr|fixity)\b`
#### 2.5. Exception Handling
-   **Description**: Keywords for managing exceptions and errors.
-   **Scope Name**: `keyword.control.exception.{keyword_name}` (e.g., `keyword.control.exception.try`)
-   **Keywords**: `try`, `catch`, `except`, `finally`, `raise`, `throw`
-   **Regex Example**: `\b(try|catch|except|finally|raise|throw)\b`
#### 2.6. Context Management
-   **Description**: Keywords for managing resource contexts.
-   **Scope Name**: `keyword.control.context.{keyword_name}` (e.g., `keyword.control.context.with`)
-   **Keywords**: `with`, `with..as`, `using`
-   **Regex Example**: `\b(with|with\.\.as|using)\b`
#### 2.7. Visibility/Access Modifiers
-   **Description**: Keywords controlling the visibility or accessibility of declarations.
-   **Scope Name**: `storage.modifier.{keyword_name}` (e.g., `storage.modifier.public`)
-   **Keywords**: `public`, `private`, `protected`, `internal`
-   **Regex Example**: `\b(public|private|protected|internal)\b`
#### 2.8. Other Semantic Keywords
-   **Description**: Miscellaneous keywords with specific semantic roles.
-   **Scope Name**: `keyword.other.{keyword_name}` (e.g., `keyword.other.async`)
-   **Keywords**: `async`, `await`, `static`, `abstract`, `final`, `sealed`, `new`, `delete`
-   **Regex Example**: `\b(async|await|static|abstract|final|sealed|new|delete)\b`
### 3. Identifiers
User-defined names for various programming constructs.
-   **Scope Prefix**: `variable` (for general variables), `entity.name` (for named entities)
#### 3.1. Variable Names
-   **Description**: Names given to variables.
-   **Scope Name**: `variable.other.readwrite` (for general variables), `variable.parameter` (for function parameters)
-   **Regex Example**: `[a-zA-Z_][a-zA-Z0-9_]*`
#### 3.2. Function/Method Calls
-   **Description**: Names of functions or methods being called.
-   **Scope Name**: `entity.name.function.call`
-   **Regex Example**: `\b[a-zA-Z_][a-zA-Z0-9_]*\s*\(` (followed by arguments and closing parenthesis)
#### 3.3. Class/Type Names
-   **Description**: Names of classes, interfaces, enums, or custom types. Typically PascalCase.
-   **Scope Name**: `entity.name.type`
-   **Regex Example**: `\b[A-Z][a-zA-Z0-9_]*\b`
#### 3.4. Module/Package Names
-   **Description**: Names used for modules or packages.
-   **Scope Name**: `entity.name.module`
-   **Regex Example**: `\b[a-zA-Z_][a-zA-Z0-9_.]*\b` (allowing dots for sub-modules)
#### 3.5. Macros
-   **Description**: Language-specific macros or decorators.
-   **Scope Name**: `support.function.macro`
-   **Regex Example**: `@\b[a-zA-Z_][a-zA-Z0-9_]*\b` (for `@` prefixed macros)
#### 3.6. Labels
-   **Description**: Targets for `goto` statements.
-   **Scope Name**: `entity.name.label`
-   **Regex Example**: `\b[a-zA-Z_][a-zA-Z0-9_]*:` (followed by a colon)
#### 3.7. Special Identifiers/Magic Methods
-   **Description**: Language-defined special identifiers or "magic" methods (e.g., dunder methods in Python).
-   **Scope Name**: `variable.language.special`
-   **Regex Example**: `__\b[a-zA-Z_][a-zA-Z0-9_]*\b__`
### 4. Literals
Fixed values directly represented in the code.
-   **Scope Prefix**: `constant` (for numbers, booleans, nulls, symbols), `string` (for text-based literals)
#### 4.1. Numbers
-   **Description**: Integer, floating-point, hexadecimal, scientific, complex, or rational numbers.
-   **Scope Name**: `constant.numeric`
-   **Regex Examples**:
    -   Integer: `\b\d+\b`
    -   Float: `\b\d+\.\d+(?:[eE][+-]?\d+)?\b`
    -   Hex: `\b0x[0-9a-fA-F]+\b`
    -   Complex: `\b\d+(?:\.\d+)?(?:[eE][+-]?\d+)?[ijm]\b` (e.g., `3 + 4j`, `1 + 2im`)
    -   Rational: `\b\d+//\d+\b`
#### 4.2. Strings
-   **Description**: Text enclosed in various types of quotes, including templated/interpolated strings.
-   **Scope Prefix**: `string.quoted`
##### 4.2.1. Double-Quoted Strings
-   **Scope Name**: `string.quoted.double`
-   **Begin Regex**: `"`
-   **End Regex**: `"`
-   **Patterns (for escapes/interpolation)**:
    -   Escape characters: `constant.character.escape` (e.g., `\\.` to match `\n`, `\"`, etc.)
    -   Interpolation: `string.interpolated` (e.g., `\{\{.*?\}\}`)
##### 4.2.2. Single-Quoted Strings
-   **Scope Name**: `string.quoted.single`
-   **Begin Regex**: `'`
-   **End Regex**: `'`
-   **Patterns (for escapes/interpolation)**: Similar to double-quoted.
##### 4.2.3. Triple-Quoted/Multi-line Strings
-   **Scope Name**: `string.quoted.triple`
-   **Begin Regex**: `"""` or `'''` or `\[\[` (for `[[multi-line]]`)
-   **End Regex**: `"""` or `'''` or `\]\]`
-   **Patterns (for escapes/interpolation)**: Similar to single/double quoted, but also allows newlines.
##### 4.2.4. Interpolated/Templated Strings
-   **Description**: Strings that can contain embedded expressions.
-   **Scope Name**: `string.interpolated` (for the embedded expression part)
-   **Regex Example**: `(?:\{[^{}]*?\})` (for `f"Hello, {name}!"`), `(?:\{\{.*?\}\})` (for `{{expression}}`)
#### 4.3. Regular Expressions
-   **Description**: Regular expression literals.
-   **Scope Name**: `string.regexp`
-   **Regex Example**: `(?<=\s|^)/[^/\n]*?/[gim]*\b` (for `/pattern/flags`), `\br"[^"\\]*(?:\\.[^"\\]*)*"` (for raw strings used as regex)
#### 4.4. Booleans
-   **Description**: Boolean true/false values.
-   **Scope Name**: `constant.language.boolean`
-   **Keywords**: `true`, `false`, `True`, `False`
-   **Regex Example**: `\b(true|false|True|False)\b`
#### 4.5. Null/None Values
-   **Description**: Representation of an empty or undefined value.
-   **Scope Name**: `constant.language.null`
-   **Keywords**: `null`, `nil`, `None`, `undefined`, `nothing`, `missing`
-   **Regex Example**: `\b(null|nil|None|undefined|nothing|missing)\b`
#### 4.6. Symbols
-   **Description**: Language-specific symbols (e.g., Ruby symbols).
-   **Scope Name**: `constant.other.symbol`
-   **Regex Example**: `:([a-zA-Z_][a-zA-Z0-9_]*|\W+)` (for `:my_symbol` or `:+`)
#### 4.7. Collection Literals
-   **Description**: Syntax for defining collections like lists, tuples, dictionaries, and sets.
##### 4.7.1. Lists/Arrays
-   **Scope Name**: `meta.structure.list` (for the entire structure)
-   **Begin Regex**: `\[`
-   **End Regex**: `\]`
-   **Patterns**: `include $self` (for nested expressions/literals)
-   **Punctuation**: `punctuation.definition.list.begin` (`[`), `punctuation.definition.list.end` (`]`)
##### 4.7.2. Tuples
-   **Scope Name**: `meta.structure.tuple`
-   **Begin Regex**: `\(`
-   **End Regex**: `\)`
-   **Patterns**: `include $self`
-   **Punctuation**: `punctuation.definition.tuple.begin` (`(`), `punctuation.definition.tuple.end` (`)`)
##### 4.7.3. Matrices
-   **Scope Name**: `meta.structure.matrix`
-   **Begin Regex**: `\[`
-   **End Regex**: `\]`
-   **Patterns**: `include $self` (for numbers and semicolons for row separation)
-   **Punctuation**: `punctuation.definition.matrix.begin` (`[`), `punctuation.definition.matrix.end` (`]`), `punctuation.separator.matrix.row` (`;`)
##### 4.7.4. Dictionaries/Maps/Objects
-   **Scope Name**: `meta.structure.dictionary`
-   **Begin Regex**: `\{`
-   **End Regex**: `\}`
-   **Patterns**: `include $self` (for keys, values, and separators)
-   **Punctuation**: `punctuation.definition.dictionary.begin` (`{`), `punctuation.definition.dictionary.end` (`}`), `punctuation.separator.key-value` (`:` or `=`), `punctuation.separator.dictionary.item` (`,`)
##### 4.7.5. Sets
-   **Scope Name**: `meta.structure.set`
-   **Begin Regex**: `\{`
-   **End Regex**: `\}`
-   **Patterns**: `include $self`
-   **Punctuation**: `punctuation.definition.set.begin` (`{`), `punctuation.definition.set.end` (`}`), `punctuation.separator.set.item` (`,`)
#### 4.8. Function/Lambda Literals
-   **Description**: Anonymous function definitions.
-   **Scope Name**: `meta.function.lambda`
-   **Regex Example**: `(?:->|lambda\b)` (for arrow functions or `lambda` keyword), followed by parameters and body.
### 5. Comments
Non-executable annotations within the code.
-   **Scope Prefix**: `comment`
#### 5.1. Single-Line Comments
-   **Description**: Comments that span a single line.
-   **Scope Name**: `comment.line.{comment_type}`
-   **Regex Examples**:
    -   `# comment`: `comment.line.number-sign` (begin: `(?<!\w)#`, end: `\n`)
    -   `// comment`: `comment.line.double-slash` (begin: `//`, end: `\n`)
    -   `-- comment`: `comment.line.double-dash` (begin: `--`, end: `\n`)
#### 5.2. Multi-Line Comments
-   **Description**: Comments that can span multiple lines.
-   **Scope Name**: `comment.block.{comment_type}`
-   **Regex Examples**:
    -   `/* multi-line */`: `comment.block.c-style` (begin: `/\*`, end: `\*/`)
    -   `#= multi-line =#`: `comment.block.hash-equals` (begin: `#=`, end: `=#`)
    -   `--[[ multi-line --]]`: `comment.block.double-dash-brackets` (begin: `--\[\[`, end: `--\]\]`)
#### 5.3. Docstrings
-   **Description**: Multi-line string literals used for documenting code, often appearing at the beginning of modules, functions, or classes.
-   **Scope Name**: `string.quoted.triple.documentation` (inherits from string for styling but indicates documentation purpose)
-   **Begin/End Regex**: `"""` or `'''` (similar to triple-quoted strings, but distinguished by context/position if possible, or by a specialized rule if not. TextMate typically treats these as strings primarily, but their "documentation" nature can be a sub-scope.)
### 6. Operators
Symbols performing operations or defining relationships.
-   **Scope Name**: `keyword.operator.{operator_type}`
-   **Arithmetic**: `+`, `-`, `*`, `/`, `//`, `%`, `**`
    -   **Scope**: `keyword.operator.arithmetic`
    -   **Regex**: `[+\-*/%]|(?<!/)/{2}|\*{2}`
-   **Assignment**: `=`, `+=`, `-=`, `:=`
    -   **Scope**: `keyword.operator.assignment`
    -   **Regex**: `=|\+=|-=|:=`
-   **Comparison**: `==`, `!=`, `>`, `<`, `>=`, `<=`
    -   **Scope**: `keyword.operator.comparison`
    -   **Regex**: `==|!=|>=|<=|>|<`
-   **Logical**: `and`, `or`, `not`, `&&`, `||`, `!`
    -   **Scope**: `keyword.operator.logical`
    -   **Regex**: `\b(and|or|not)\b|&&|\|\||!`
-   **Bitwise**: `&`, `|`, `^`, `~`, `<<`, `>>`
    -   **Scope**: `keyword.operator.bitwise`
    -   **Regex**: `&|\||\^|~|<<|>>`
-   **Membership/Identity**: `in`, `is`, `typeof`
    -   **Scope**: `keyword.operator.membership`
    -   **Regex**: `\b(in|is|typeof)\b`
-   **Member Access**: `.`, `::` (for namespacing/static access)
    -   **Scope**: `punctuation.separator.access`
    -   **Regex**: `\.|\::{2}`
-   **Indexing/Slicing**: `[]` (as an operator, distinct from collection literal brackets)
    -   **Scope**: `punctuation.accessor.array`
    -   **Regex**: `\[|\]`
-   **Other/Special**: `?` (ternary, null-coalescing), `...` (spread, rest, variadic), `.` (broadcasting), `del`
    -   **Scope**: `keyword.operator.other`
    -   **Regex**: `\?|\.{3}|\bdel\b`
### 7. Punctuation/Delimiters
Structural elements that organize code.
-   **Scope Prefix**: `punctuation`
-   **Parentheses**: `()`
    -   **Scope**: `punctuation.paren.open`, `punctuation.paren.close`
    -   **Regex**: `\(`, `\)`
-   **Brackets**: `[]`
    -   **Scope**: `punctuation.bracket.square.open`, `punctuation.bracket.square.close`
    -   **Regex**: `\[`, `\]`
-   **Braces**: `{}`
    -   **Scope**: `punctuation.bracket.curly.open`, `punctuation.bracket.curly.close`
    -   **Regex**: `\{`, `\}`
-   **Commas**: `,`
    -   **Scope**: `punctuation.separator.comma`
    -   **Regex**: `,`
-   **Semicolons**: `;`
    -   **Scope**: `punctuation.separator.delimiter`
    -   **Regex**: `;`
-   **Colons**: `:`, `::` (as delimiter, distinct from operator)
    -   **Scope**: `punctuation.separator.colon`, `punctuation.separator.double-colon`
    -   **Regex**: `:` (single colon), `::` (double colon)
-   **Dots**: `.`, `...` (as delimiter, distinct from operator)
    -   **Scope**: `punctuation.separator.dot`, `punctuation.separator.ellipsis`
    -   **Regex**: `\.` (single dot), `\.{3}` (ellipsis)
-   **Block Structures (Keywords)**: `begin`, `end`, `do`, `done`, `then`, `fi`, `esac`
    -   **Scope**: `keyword.control.block-start`, `keyword.control.block-end`
    -   **Regex**: `\b(begin|do|then)\b` (start), `\b(end|done|fi|esac)\b` (end)
-   **Statement Terminators**: `\n` (newline, if significant), `;`
    -   **Scope**: `punctuation.terminator.statement`
    -   **Regex**: `;` (for explicit semicolon), `\n` (for implicit newline termination if the grammar handles it explicitly)
### 8. Indentation (Whitespace)
-   **Description**: Leading whitespace that defines code blocks. TextMate handles this implicitly for folding, but explicit scoping is not typically applied to indentation characters themselves for highlighting. It is recognized as structural.
-   **Scope Name**: Not a direct tokenizable element for typical highlighting; managed by TextMate's internal folding/indentation rules.
### 9. Directives/Annotations
Language-specific constructs that influence compilation, interpretation, or runtime behavior.
-   **Scope Prefix**: `meta.preprocessor` (for pre-processor directives), `meta.annotation` (for annotations)
-   **Pre-processor Directives**: `#include`, `#define`, `#ifdef`
    -   **Scope Name**: `meta.preprocessor.{directive_name}` (e.g., `meta.preprocessor.include`)
    -   **Regex Example**: `^#\s*(include|define|ifdef)\b.*`
-   **Annotations/Decorators**: `@annotation`, `#[attribute]`
    -   **Scope Name**: `meta.annotation.{annotation_name}` (e.g., `meta.annotation.decorator`)
    -   **Regex Example**: `@\b[a-zA-Z_][a-zA-Z0-9_]*\b` (for Python-style decorators), `\#\[.*?\]` (for Rust-style attributes)
-   **Pragmas**: `pragma once`
    -   **Scope Name**: `meta.pragma`
    -   **Regex Example**: `\bpragma\b.*`
### 10. Type Annotations/Hints
Syntax used to specify types, typically for static analysis or documentation.
-   **Scope Prefix**: `storage.type`
-   **Type Declarations**: `name: str`, `x::Int`
    -   **Scope Name**: `storage.type.annotation` (for the type name itself within an annotation)
    -   **Regex Example**: `(?::|\b::\b)\s*([A-Z][a-zA-Z0-9_]*)` (for type names like `str`, `Int`)
-   **Function Return Types**: `-> str`
    -   **Scope Name**: `storage.type.function.return`
    -   **Regex Example**: `->\s*([A-Z][a-zA-Z0-9_]*)`
-   **Type Parameters/Generics**: `<T>`, `[U]`
    -   **Scope Name**: `storage.type.generic`
    -   **Regex Example**: `<[A-Z][a-zA-Z0-9_]*>|\[[A-Z][a-zA-Z0-9_]*\]`
