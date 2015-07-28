Terms
-----
(abstraction: Thinking in chunks, so that you can fit larger problems in your mind at once)
(data abstraction: the invention of new data types)
(abstract data type: technical term for an invented data type)
(selector: a way to take lists apart, {car, cdr})
(constructor: a way to construct a list, {list, cons})
(rest parameter: a parameter that follows a dot that represents a variable number of arguments)

Notes
-----
* {List} introduced
* Sentences were introduced to Scheme for this book, where as lists are at the core of Lisp.
* car -> first
* cdr -> butfirst
* null? -> empty?
* list is used to create a new list
* cons is used to add to an existing list
* cons <element> <list>
* append <list> <list>
* Discussion of why sentence, word, first, last, etc were used to deal with text.
* every -> map
* keep -> filter
* accumulate -> reduce
* list?
* equal?
* member? only works for words and sentences -> member, second arg is a list, and returns portion of list that matches.
* item -> list-ref. (list-ref <list> <zero-based-index)
* count -> length
* (assoc <name> <name-list) used for association lists
* . used for taking a variable number of arguments
* The number of formal parameters before a dot determines the minimum number of arguments that must be used when a procedure is invoked.
* There can only be one parameter after the dot.
* 'deep' recursion examples given for appearance and pigl
* car-cdr recursion is used in the deep-pigl definition.
