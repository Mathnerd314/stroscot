Exemplary programs
##################

A programming language is no good unless some significant programs have been written in it. This is mainly so that dark corners of the language have been explored and debugged, but there is also a "halo effect" of "I like program X, therefore I like programming language L that X is written in". Also, every popular PL solves a specific set of programming problems really well, in specific domains. So get the set of problems right, and the design for the PL will reveal itself naturally. Or solve all the programming problems and win the game. Once Stroscot gets users it can switch to problem-driven design: work with real-world users who have problems, abstract and implement solutions for them.


Social network
--------------

A social network is a large program for any web development / app development framework to cut its teeth on. For example, the Hack programming language was closely associated with Facebook.

Basic features copied from Reddit
- creating a post
- replying to a post (creates its own post)
- reacting to a post (upvote, downvote, save, hide)

As far as theme, Goodreads has received a lot of criticism and there might be room to replace it.

Terminal / text editor
----------------------

After reading about elastic tabstops I've always thought that there's a better solution, "tablike spaces". The idea here is to use a normal proportional font for rendering, but to make the spaces jump to the pixel column they would use if the font was monospaced. So rendering "a bit of text" would render "a" at 0, "bit" at 2 ems, "of" at 6 ems, and "text" at 9 ems, where an em is the width of the widest character in the font.

A more complex algorithm treats the text as a giant table, so "a bit of text" gets split up into 4 cells "a ", "bit ", "of ", "text" which span 2,4,3,4 columns respectively. Then the column widths are calculated using the `auto table layout algorithm <https://www.w3.org/TR/CSS2/tables.html#auto-table-layout>`__ (simplified):

* Set the width of each column to 0.
* For each cell, calculate the width as rendered by the font, and increase the widths of the columns it spans so that together, they are at least as wide as the cell. Widen all spanned columns to be approximately the same.

Yet more complex is to treat it as a constraint problem. The constraints consist of minimum width constraints from the width of the tokens and order constraints that specify which chunks of text are before/after/line up with other chunks. The goal is to minimize the width of the table (sum of column widths), and as a secondary objective make the widths as uniform as possible (lowest standard deviation or absolute deviation). The Cassowary algorithm might work.

Other ideas
-----------

Trading engine (Stock, options, cryptocurrency...)
Game engine
UI toolkit
Raytracer
Vector graphics library (bonus points for supporting SVG drawings and OpenType fonts)
HPC simulation (wind tunnel, nukes, ...)
abstract assembly (like High Level Assembly)
expression problem (compiler)
inventory tracker for android (like todo-mvc but better)