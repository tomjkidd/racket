#lang racket

(require "../full-core.rkt")

(rember-f-first o= 5 '(6 2 5 3))
(rember-f-first eq? 'jelly '(jelly beans are good))
(rember-f-first equal? '(pop corn) (list 'lemonade '(pop corn) 'and '(cake)))

(define eq?-salad (eq?-c 'salad))
(eq?-salad 'salad)
(eq?-salad 'tuna)
((eq?-c 'salad) 'tuna)
(rember-eq? 'tuna '(tuna salad is good))
((rember-f eq?) 'tuna '(shrimp salad and tuna salad))
((rember-f eq?) 'eq? '(equal? eq? eqan? eqlist? eqpair?))
((insertR-f eq?) 10 1 '(1 2 3))
((insertL-f eq?) 10 1 '(1 2 3))
((insertR-f equal?) 'car 1 '(1 2 3))
((insertL-f equal?) 'car 1 '(1 2 3))
((subst-f equal?) 'car 1 '(1 2 3))
((rember-f2 equal?) 1 '(1 2 3))

#|
The Ninth Commandment
Abstract common patterns with a new function
|#