from dataclasses import dataclass, field
from typing import List, Union, Dict, Any

# Predefined list of special built-in functions
BUILT_IN_FUNCTIONS = [
    "Choice", "OneOf", "AllOf", "Exists", "ForAll", "Not", "And", "Or", "Equal", "Failure",
    # Functions for memory management
    "Allocate", "GetMemory", "SetMemory",
    # Functions for concurrency
    "SpawnThread", "JoinThread",
    # Functions for foreign language interaction
    "CallForeignFunction", "MarshalData"
]

@dataclass
class Node:
    """Base class for all AST nodes."""
    pass

@dataclass
class Program(Node):
    """
    The main blueprint for your code, containing all instructions and definitions.
    The program entry point is a special expression.
    """
    modules: List['Module'] = field(default_factory=list) # A Program can contain multiple Module nodes.
    global_declarations: List['Definition'] = field(default_factory=list) # A Program can contain global declarations.
    entry_point: 'Expression|None' = None # The program entry point is a special expression.

@dataclass
class NamePathNode(Node):
    """Helps refer to things using a clear path of names."""
    path: List[str] # A clear path of names.

@dataclass
class ImportNode(Node):
    """Allows shortening NamePathNodes."""
    path: NamePathNode # The path to shorten.
    alias: str|None = None # Optional alias for the imported path.

@dataclass
class EncapsulationNode(Node):
    """Groups definitions and can restrict access."""
    definitions: List[Node] # Groups definitions.
    access_restrictions: Any = None # Can restrict what other parts of the code can access inside them.

@dataclass
class IdentifierNode(Node):
    """Names used for various parts of the code."""
    name: str # The name.

@dataclass
class Module(Node):
    """
    Organizes code, has a name and contains its own definitions.
    Modules can be nested.
    """
    name: str # Each Module has a name.
    definitions: List['Definition'] = field(default_factory=list) # Contains its own definitions.
    modules: List['Module'] = field(default_factory=list) # Modules can even be placed inside other modules.

@dataclass
class Definition(Node):
    """Base class for various definitions within the language."""
    name: IdentifierNode # The name of the definition.

@dataclass
class Expression(Node):
    """A piece of code that can be evaluated and produces a value."""
    pass

@dataclass
class Symbol(Expression):
    """An expression that is just a name made of characters."""
    name: str # A name made of characters.

@dataclass
class Literal(Expression):
    """Basic values like numbers, true/false, or text."""
    value: Union[int, float, bool, str] # Basic values.

@dataclass
class Term(Expression):
    """A symbol applied to multiple expressions (term rewriting)."""
    symbol: Symbol # A symbol.
    expressions: List[Expression] # Applied to multiple expressions.

@dataclass
class Tuple(Expression):
    """A collection of values."""
    elements: List[Expression] # A collection of values.

@dataclass
class Application(Expression):
    """Applying a function to its arguments, curried (one at a time)."""
    function: Expression # The function to apply.
    argument: Expression # The argument for the curried application.

@dataclass
class FunctionCall(Expression):
    """Calling a function with its inputs (arguments)."""
    function: Expression # The function to call.
    arguments: List['Argument'] = field(default_factory=list) # Providing the function and its inputs (arguments).

@dataclass
class Argument(Node):
    """Holds values passed to a function and can indicate partial application."""
    value: Expression # Holds the values you pass to a function.
    is_partial: bool = False # Can also indicate that you're only partially providing arguments (for "currying").

@dataclass
class Abstraction(Expression):
    """Creates unnamed functions with a variable and a body of code (Lambda Calculus)."""
    variable: IdentifierNode # A variable.
    body: Expression # A body of code.

@dataclass
class Variable(Expression):
    """Expressions consisting of names that refer to values or other definitions."""
    name: IdentifierNode # Names that refer to values or other definitions.

@dataclass
class LetBinding(Expression):
    """Defines local variables that only exist within a specific part of your code."""
    variable: IdentifierNode # Defines local variables.
    value: Expression # The value assigned to the variable.
    body: Expression # The specific part of your code where it exists.

@dataclass
class SetDefinition(Definition):
    """Base class for defining a set."""
    pass

@dataclass
class SetLiteral(SetDefinition):
    """A direct list of values in a set."""
    elements: List[Expression] # A direct list of values.

@dataclass
class SetBuilderNotation(SetDefinition):
    """Defines a set based on a logical rule."""
    expression: Expression # The expression for the elements of the set.
    qualifiers: List['Qualifier'] = field(default_factory=list) # The logical rule (represented by qualifiers).

@dataclass
class SetOperation(Expression):
    """Performs standard set operations."""
    operation_type: str # e.g., "union", "intersection", "complement", "difference".
    operaands: List['Expression'] # The operands for the operation, which can be sets or expressions.

@dataclass
class MembershipTest(Expression):
    """Checks if a value is part of a set."""
    value: Expression # The value to check.
    set_expression: Expression # The set to check against.

@dataclass
class TypeAnnotationAssertion(Node):
    """Declares that a value belongs to a specific set (which can act as a type)."""
    value: Expression # A value.
    set_expression: Expression # Belongs to a specific set.

@dataclass
class SetDeclaration(Definition):
    """Defines module-level sets in an extensible way across the program."""
    name: IdentifierNode # The name of the set.
    definition: SetDefinition # The definition of the set.

@dataclass
class ExceptionDefinition(SetDeclaration):
    """Defines an exception via a special set and handling functions."""
    handling_functions: List['FunctionDefinition'] = field(default_factory=list) # Handling functions.

@dataclass
class Object(Expression):
    """A special type of record which groups together data (properties) and actions (methods)."""
    properties: Dict[str, Expression] = field(default_factory=dict) # Groups together data (properties).
    methods: Dict[str, 'FunctionDefinition'] = field(default_factory=dict) # Groups together actions (methods).

@dataclass
class PropertyAccess(Expression):
    """Lets you get to the data within an object."""
    obj: Expression # The object.
    property_name: IdentifierNode # The data within an object.

@dataclass
class MethodCall(Expression):
    """Lets you tell an object to perform one of its actions."""
    obj: Expression # The object.
    method_name: IdentifierNode # One of its actions.
    arguments: List[Argument] = field(default_factory=list) # Arguments for the method call.

@dataclass
class MonadComprehension(Expression):
    """Builds up results by combining an expression with "qualifiers"."""
    expression: Expression # An expression.
    qualifiers: List['Qualifier'] = field(default_factory=list) # Qualifiers.

@dataclass
class Qualifier(Node):
    """Base class for different types of qualifiers in a Monad Comprehension."""
    pass

@dataclass
class Generator(Qualifier):
    """Takes a value from an expression and binds it to a pattern."""
    pattern: 'Pattern' # Binds it to a pattern.
    expression: Expression # Takes a value from an expression.

@dataclass
class FilterGuard(Qualifier):
    """A true/false expression that filters results."""
    condition: Expression # A true/false expression that filters results.

@dataclass
class LetBindingQualifier(Qualifier):
    """Defines local variables within the comprehension."""
    variable: IdentifierNode # Defines local variables.
    value: Expression # The value assigned to the variable.

@dataclass
class ParallelComprehension(Qualifier):
    """Combines results from multiple independent qualifiers."""
    qualifiers: List[Qualifier] = field(default_factory=list) # Multiple independent qualifiers.

@dataclass
class TransformComprehension(Qualifier):
    """Applies a function to the generated values, with optional grouping."""
    function: Expression # Applies a function to the generated values.
    group_by_expression: Expression|None = None # Optional `group_by_expression` for grouping results.

@dataclass
class Statement(Node):
    """Instructions that tell the program to do something."""
    pass

@dataclass
class Block(Node):
    """A group of statements."""
    statements: List[Statement] = field(default_factory=list) # A group of statements.

@dataclass
class SimpleStatement(Statement):
    """An expression used as an instruction."""
    expression: Expression # An expression.

@dataclass
class NonReturningStatement(Statement):
    """Performs an action but doesn't produce a value you use later."""
    action: Expression # Performs an action.

@dataclass
class ReturningStatement(Statement):
    """Performs an action and then gives you back a value, which you can assign to a "pattern"."""
    action: Expression # Performs an action.
    pattern: 'Pattern' # Assign to a "pattern".

@dataclass
class EmptyBlock(Block):
    """Does nothing."""
    pass # Does nothing.

@dataclass
class ReturnStatement(Statement):
    """Specifically sends a value back from a block."""
    value: Expression # Sends a value back from a block.

@dataclass
class Pattern(Node):
    """Base class for patterns, used to control how code runs based on data."""
    pass

@dataclass
class WildcardPattern(Pattern):
    """Matches anything."""
    pass # Matches anything.

@dataclass
class AtomMatchPattern(Pattern):
    """Matches a specific basic value."""
    value: Literal # Matches a specific basic value.

@dataclass
class ListStartEndPattern(Pattern):
    """Matches lists that start and end with certain values."""
    start_elements: List[Pattern] = field(default_factory=list) # Matches lists that start with certain values.
    end_elements: List[Pattern] = field(default_factory=list) # Matches lists that end with certain values.
    middle_pattern: Pattern|None = None # Optional pattern for elements in the middle.

@dataclass
class AndPattern(Pattern):
    """Combines other patterns using logical AND."""
    patterns: List[Pattern] = field(default_factory=list) # Combining other patterns.

@dataclass
class OrPattern(Pattern):
    """Combines other patterns using logical OR."""
    patterns: List[Pattern] = field(default_factory=list) # Combining other patterns.

@dataclass
class Clause(Node):
    """A pair of a "pattern" and a "body" of code."""
    pattern: Pattern # A "pattern".
    body: Block # A "body" of code.

@dataclass
class FunctionDefinition(Definition):
    """Defines a function by a series of clauses."""
    name: IdentifierNode # The name of the function.
    clauses: List[Clause] = field(default_factory=list) # Runs clauses.

@dataclass
class MacroDefinition(Definition):
    """Defines "macros," which are clauses that manipulate code as abstract syntax."""
    name: IdentifierNode # The name of the macro.
    clauses: List[Clause] = field(default_factory=list) # Clauses that manipulate code as abstract syntax.

@dataclass
class ASTNode(Expression):
    """Represents a piece of code wrapped so that it isn't evaluated."""
    code: Node # A piece of code wrapped so that it isn't evaluated.

@dataclass
class Evaluate(Expression):
    """Runs transformed code from a macro."""
    ast_node: ASTNode # Runs the transformed code.