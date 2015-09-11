Terms
=====
(#s: A goal that succeeds)
(#u: A goal that fails)
(unify: To associate variables)
(fresh: When a variable has no association)
(co-refer: When one variable is associated with another, also called share)
(reification: The process by which an abstract idea about a computer program is turned into an explicit data model or other object in a programming language)

Notes
=====
[The Little Schemer]() was brought up casually.

[Lambda the Ultimate]() was brought up, also.

George Boole, of Boolean fame

Thoralf Albert Skolem

Jacques Herbrand

William F. Clocksin. Clause and Effect, conde

run returns () when #u is the result goal.

unify succeeds when either of its arguments is fresh.
unify is associative

Encountered
===========
{symbol, name, description}
{#s, succeed, A goal that succeeds}
{#u, fail, A goal that fails}
{run*, run, The interface to scheme of the logic programming extension. Its arguments are a series of goals. It returns a list if its goals succeed}
{==, unify, Associates two variables to eachother}
{fresh, fresh, (fresh (f ...) g ...) binds fresh variables to f ... and succeeds if the goals g... succeed.}
{_.0, (reify-name 0), A symbol representing a fresh variable}
{conde, conde, The "e" stands for every line.}
