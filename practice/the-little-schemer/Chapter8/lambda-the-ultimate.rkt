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
((rember-f2 equal?) #f '(#t #f #t #f))
#|
The Ninth Commandment
Abstract common patterns with a new function
|#
(atom-to-function (operator '(1 + 2)))
(value '(1 + 2))
(value '(2 * 3))
(value (list 1 '+ '(3 expt 4)))

((multirember-f eq?) 1 '(1 2 1 3 1))
((multirember-f equal?) 'a '(a 1 2 3 a b c a d))
((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna))
(multirember-eq? 'tuna '(shrimp salad tuna salad and tuna))

(define eq?-tuna
  (eq?-c 'tuna))

(eq?-tuna 'bass)
(eq?-tuna 'tuna)

(multiremberT eq?-tuna '(shrimp salad tuna salad and tuna))

(define a-friend
  (lambda (x y)
    (null? y)))

(define 2nd-arg-null? a-friend)

(multirember&co 'tuna '(strawberries tuna and swordfish) 2nd-arg-null?)
(multirember&co 'tuna '() 2nd-arg-null?)
(multirember&co 'tuna '(tuna) 2nd-arg-null?)
(multirember&co 'tuna '(and tuna) 2nd-arg-null?)

(define new-friend
  (lambda (newlat seen)
    (a-friend newlat
         (cons 'tuna seen))))

(define latest-friend
  (lambda (newlat seen)
    (a-friend (cons '(and) newlat)
              seen)))

(define last-friend
  (lambda (x y)
    (length x)))

(multirember&co 'tuna '(strawberries tuna and swordfish) last-friend)

(multirember&co 'tuna '(tuna bacon tuna beef)
                (lambda (x y)
                  (and (eqlist? x '(bacon beef))
                       (eqlist? y '(tuna tuna)))))

#|
The Tenth Commandment
Build functions to collect more than one value at a time
|#