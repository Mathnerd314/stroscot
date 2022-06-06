Modules
#######

Java has four access modes:
private - abstract data type, structure not accessible to the outside world. use exported functions to manipulate.
public - All data is directly accessible
protected - a variant of public, because anybody can access a protected member if he or she wants to, without much difficulty.
package - a variant of private, limiting access to the package rather than the file. Intended to replace C++ "friendly"

- exports: we write: 'export a, b, c;' (with a, b, c, etc.  a list of
  exported names, possibly also: structure.field)
- the ordering of methods in interfaces is not relevant

1ML
===

The module system is a superset of 1ML, MixML, Backpack, and Newspeak's module system, so those don't need to be considered further. (TODO: reread the papers to check this. but they're mostly type systems, rather than functionality)


Later in the pipeline:
* A similarly-named identifiers warning based on `confusables <http://www.unicode.org/reports/tr39/#Confusable_Detection>`_

the `dreaded <https://2ality.com/2011/06/with-statement.html>`__ ``with`` statement

how does module import work without "with"?

Dependency hell
===============

It is key to support side-by-side execution of multiple versions of the same functionality. So names also include version numbers and a cryptographic hash.

Lexical scoping
===============

An identifier is visible from the point of its declaration to the end of the immediately surrounding block.
Later identifiers shadow earlier identifiers with the same name; shadowing generates a warning.
There is only one effective scope at any point in the code (for instance, variables and types are looked up in the same scope).

Uniform scoping rules make programs easier to read and to understand.
