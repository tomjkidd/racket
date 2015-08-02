Terms
-----
(side effects: an action that changes something)
(sequencing: the order in which actions are executed)
(function: something that computes and returns one value, with no side effects)
(procedure: general term for what lambda returns, an embodiment of an algorithm)
(independent: a set of expressions that don't help compute one another)

Notes
-----
* {show, display, newline, show-list, for-each, read, read-line, show-line, align} functions introduced
* When invoking a procedure with a sequence, Scheme evaluates all the expressions in the body, in order, and returns the valu of the last one.
* {begin} special form used with if to group together expressions.
* Formatting output is discussed.
* Let can be used to ensure order of evaluation.
* While working with the tic tac toe code, it became clear that I wanted to
put that code in its own file in order to work with it in isolation, and
then provide it as a [racket module](http://docs.racket-lang.org/guide/module-basics.html) for better
organization.
