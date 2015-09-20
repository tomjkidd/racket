#lang racket

(require "../core.rkt")

(define l1 (list '(coffee)
                    'cup
                    (list '(tea) 'cup)
                    (list 'and '(hick)) 'cup))

(define l2 (list (list '(tomato sauce))
                     (list '(bean) 'sauce)
                     (list 'and (list '(flying)) 'sauce)))

(rember* 'cup l1)

(rember* 'sauce l2)

(lat? l2)
(atom? (car l2))

(define l3 (list (list 'how 'much '(wood))
                 'could
                 (list (list 'a '(wood) 'chuck))
                 (list (list '(chuck)))
                 (list 'if '(a) (list '(wood chuck)))
                 'could 'chuck 'wood))
(insertR* 'roast 'chuck l3)

#|
The First Commandment
When recurring on a list of atoms, lat, ask two questions about it: (null? lat) and else
When recurring on a number, n, ask two questions about it: (zero? n) and else
Wher recurring on a list of S-expressions, l, ask three questions about it:
(null? l), (atom? (car l)), and else
|#

#|
The Fourth Commandment
Always change at least one argument while recurring.
When recurring on a list of atoms, lat, use (cdr lat).
When recurring on a number, n, use (sub1 n).
When recurring on a list of S-expressions, l, use (car l) and (cdr l)
if neither (null? l) nor (atom? (car l)) are true.

It must be changed to be closer to termination. The changing argument must
be tested in the termination condition:

when using cdr, test termination with null? and
when using sub1, test termination with zero?.
|#
(define l4 (list '(banana)
                       (list 'split (list (list (list '(banana ice)))
                                          (list 'cream '(banana))
                                          'sherbet))
                       '(banana)
                       '(bread)
                       '(banana brandy)))
(occur* 'banana l4)
(subst* 'orange 'banana l4)

(insertL* 'pecker 'chuck l3)
(define l5 (list '(potato)
                 (list 'chips (list '(with) 'fish)
                       '(chips))))
(member* 'chips l5)
(leftmost l5)
(leftmost (list (list '(hot)
                      (list 'tuna '(and))
                      'cheese)))
#|(leftmost (list (list (list '()))
                17
                '(seventeen)))|#
;;(leftmost '())
(define l6 '(mozzarella pizza))
(and (atom? (car l6))
     (eq? (car l6) 'pizza))

(define l7 (list '(mozzarella mushroom) 'pizza))
(and (atom? (car l7))
     (eq? (car l7) 'pizza))

(define l8 '(pizza))
(and (atom? (car l8))
     (eq? (car l8) 'pizza))

(display 'eqlist?)
(newline)

(eqlist? '(strawberry ice cream)
         '(strawberry ice cream))

(eqlist? '(strawberry ice cream)
         '(strawberry cream ice))

(eqlist? (list 'banana (list '(split)))
         (list '(banana) '(split)))

(eqlist? (list 'beef (list '(sausage)) (list 'and '(soda)))
         (list 'beef (list '(salami)) (list 'and '(soda))))

(eqlist? (list 'beef (list '(sausage)) (list 'and '(soda)))
         (list 'beef (list '(sausage)) (list 'and '(soda))))

(display 'tests)
(newline)
(eqlist? '() '())
(eqlist? '() '(bar))
(eqlist? '() (list '(test)))
(eqlist? '(foo) '())
(eqlist? '(foo) '(foo))
(eqlist? '(foo) (list '(foo)))
(eqlist? (list '(foo) 'bar) '())
(eqlist? (list '(foo) 'bar) '(bar))
(eqlist? (list '(foo) 'bar) (list '(foo) 'bar))

#|
The Sixth Commandment
Simplify only after the function is correct.
|#

