#lang racket

(require "../core.rkt")

(atom? 14)
(number? -3)
(number? 3.14159)
(add1 67)
(sub1 5)
(sub1 0) ;; -1, but No answer for our purposes
(zero? 0)
(zero? 1492)
(o+ 46 12)
(o- 14 3)
(o- 17 9)
(o- 18 25) ;; -7, but No answer for our purposes
(tup? '(2 11 3 79 47 6))
(tup? '(8 55 5 555))
(tup? '(1 2 8 apple 4 3))
(tup? (list 3 '(7 4) 13 9))

(tup? '())
(addtup '(3 5 2 8))
(addtup '(15 6 7 12 3))

#|
The First Commandment:
When recurring on a list of atoms, lat,
ask two questions about it: (null? lat) and else
When recurring on a number, n,
ask two questions about it: (zero? n) and else
|#

(o* 5 3)

#|
The Fourth Commandment
Always change at least one argument while recurring.
It must be changed to be closer to termination. The changing
argument must be tested in the termination condition:
when using cdr, test termination with null?
when using sub1, test termination with zero?
|#

(o* 12 3)

#|
The Fifth Commandment
When building a value with +, always use 0 for the value of the terminating line,
for adding 0 does not change the value of an addition.

When building a value with *, always use 1 for the value of the terminating line,
for multiplying by 1 does not change the value of a multiplication.

When building a value with cons, always consider () for the value of the terminating
line.
|#

(tup+ '(3 6 9 11 4) '(8 5 2 0 7))
(tup+ '(3 7) '(4 6))
(tup+ '(3 7) '(4 6 8 1))

(o> 12 133)
(o> 120 11)
(o> 3 3)
(o< 8 3)
(o< 6 6)

(o= 3 2)
(o= 2 3)
(o= 3 3)
;; NOTE: Use o= for number equality and eq? for atoms that aren't numbers

(oexpt 1 1)
(oexpt 2 3)
(oexpt 5 3)
(o/ 15 4)

(length '(hotdogs with mustard sauerkraut and pickles))
(length '(ham and cheese on rye))
(pick 4 '(lasagna spaghetti raioli macaroni meatball))
;; (pick 0 '(a)) ;; No answer
(rempick 3 '(hotdogs with hot mustard))

(number? 'a)
(number? 76)
(no-nums '(5 pears 6 prunes 9 dates))

(all-nums '(5 pears 6 prunes 9 dates))

(eqan? 5 6)
(eqan? 6 6)
(eqan? 'a 'b)
(eqan? 'a 'a)
(occur 'a '(b c d))
(occur 'a '(a b c d a))
(occur 1 '(2 3 4))
(occur 1 '(1 2 3 1 2 1))
(one? 1)
(one? 0)