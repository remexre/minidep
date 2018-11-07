minidep
=======

A small dependently typed language.

TODOs
-----

 - Perform normalization in unification
 - Add eliminators to stdlib
 - Convert `unification` AST to `explicit` AST; this is the same structurally, but it doesn't have a `unificationVar` nonterminal.
 - Typecheck `explicit` AST; in principle, standard HM ought to work?

Maybe TODOs?
------------

 - Lower `explicit` AST to STLC?
 - Compile STLC to Go?

Extension TODOs
---------------

 - Typeclasses
 - Type declarations
