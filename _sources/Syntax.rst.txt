Syntax
######

The basics are numbers, booleans, and character strings of text.
But there's always more.

Arithmetic and Parentheses
==========================

The default arithmetic type is a 'number' type.
This is arbitrary-precision magic which can store just about anything.
You can use 'integer' for a strict integer, 'decimal' for banking, and
'float' for any IEEE 754 thing.

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

Stroscot supports records, arrays, and arbitrary atoms as well.
