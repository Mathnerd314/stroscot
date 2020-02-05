Syntax
######

The basics are numbers, booleans, and character strings of text.
But there's always more.

Arithmetic and Parentheses
==========================

Stroscot supports your typical arithmetic operations:

::

   pi_approx = 3+1/(7+1/(15+1/1))
   # 355/113 = 3.14159292035...

For brevity, leading and trailing parentheses can be omitted:

::

   pi_approx = 3+1/(7+1/(15+1/1
   # 355/113

If you don't like this, you can set Stroscot to warn or error on
unmatched parentheses.

::

   stroscot -Wunmatched_parentheses pi_approx
   # Warning: unmatched parentheses
   # 355/113
   stroscot -Eunmatched_parentheses pi_approx
   # Error: unmatched parentheses
   # Aborted.

A gentler approach is to reformat the code to add parentheses:

::

   stroscot -Funmatched_parentheses pi_approx
   # Log: fixing unmatched parentheses

This option is on by default and running a source code formatter is
always good practice.

Strings
===============

::

   "Hello world!"
   ``Hello user ${id}``
   [Enclosed text]
   'short'

Concatenation is ``+``. Most operators are textual:

::

   true and false = false
   true or false = true
   true xor true = false
   5 mod 2 = 1
   raise x by 1

::

   if true then 1 else 2 = 1
   repeat while x > 0 { x -= 1 }
   repeat until x == 0 { x -= 1 }
   repeat 10 times { x -= 1 }
   repeat { x -= 1 } while x > 0

::

   procedure foo
     x = 0
     x += 1
     provide x

   obtain http_server
   procedure main
     parse_args
     build_folder
     http_server.serve(folder)

::

   check {
     risky_procedure
   } error {
     fix(error) or error("wtf")
   } regardless {
     save_logs
   }

::

   x = input number
   display x

::

   // comment
   /* multiline
   comment */

Structured Data
===============

Stroscot supports records, arrays, and arbitrary atoms as well. A record of functions is called a vtable. Structural subtyping of records is supported: you can pass ``{a: 1, b: 2}`` to a function expecting only ``{b: Int}``.

Parsing
=======

I've got a basic Earley algorithm working for now. But eventually I'm extending it with BSRs and layout and `mix <http://www.cse.chalmers.se/~nad/publications/danielsson-norell-mixfix.pdf>`__\ `fix <http://www.bramvandersanden.com/publication/pdf/sanden2014thesis.pdf>`__ and other fun things. There's also `Yakker <https://github.com/attresearch/yakker>`__, which is the most developed parser I've seen feature-wise. It's only missing incremental parsing.

  A new parsing engine, Yakker, capable of handling the requirements of modern applications including full scannerless context-free grammars with regular expressions as right-hand sides for defining nonterminals. Yakker also includes facilities for binding variables to intermediate parse results and using such bindings within arbitrary constraints to control parsing. Yakker supports both semantic actions and speculative parsing techniques such as backtracking and context-free lookahead and several parsing back ends (including Earley, GLR and backtracking).  In addition, nonterminals may be parameterized by arbitrary values, which gives the system good modularity and abstraction properties in the presence of data-dependent parsing. Finally, legacy parsing libraries, such as sophisticated libraries for dates and times, may be directly incorporated into parser specifications.

Operator precedence will be a DAG, rather than levels.::

  precedence _*_ higher than _+_
  precedence _/_ equals _*_

I've looked at various algorithms but I think the only way to handle it completely correctly and generically is to have a disambiguating pass on an ambiguous parse tree. The alternatives involve generating extra parser states or using PEGs. But PEGs have big issues with error detection and reporting, not to mention correct parsing. There's just no information on what possible parses are available or what token is expected. Whereas with Earley you can do "Ruby slippers": scan the sets for what they want next, output "warning: expected ';' at end of statement", and then add that to the parse forest and continue parsing with almost no overhead.

Treesitter implements incremental LR parsing with error recovery, but since it doesn't support ambiguity I don't think it's sufficient for a compiler.
