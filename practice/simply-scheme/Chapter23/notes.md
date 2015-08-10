Terms
-----
(vector: a data structure that can be thought of as a row of boxes into which values can be put. The chapter used a set of lockers to illustrate this point. A vector is another name for an array.)
(mutator: a procedure that changes the value of some previously created data structure)
(destructive: a procedure that modifies its argument)
(constant time operations: operations that can be done in one small unit of time, regardless of the number of elements in the aggregate)
(sequential/imperative programming: a style of programming that uses state, sequence, and effects. Contrast to functional programming, which uses none of the three.)

Notes
-----
* {make-vector, vector-set!, vector-ref} for create, update, get with vectors.
* zero-index used for working with vectors.
* {vector, vector-length, vector?}
* List programming is characterized by two operations: dividing list into it's first element and all the rest, and sticking one new element onto the front of a list.
* Vector programming is characterized by selecting elements in any order, from a collection whose size is set permanently when the vector is created.
* {cons, car, cdr, null?} are list constant time operations.
* {vector-ref, vector-set!, vector-length} are vector constant time operations
