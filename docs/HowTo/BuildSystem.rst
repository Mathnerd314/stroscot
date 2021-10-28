Build system tricks
###################

Ignore a change
===============

Suppose you add a new function to a header file that many other files depend on. Most files that include it do not need to be recompiled. Solution:

* Make the changes in the header file
* Compile the files that depend on the new function
* Do a full build with ``--assume-old-touch $header``.

Find dependencies
=================

The assume new and dry run flags in combination show you what would rebuild if you were to modify that file.

