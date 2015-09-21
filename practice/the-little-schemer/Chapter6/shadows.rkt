#lang racket

(require "../core-6.rkt")
(require "../2nd-rep-6.rkt")

(aexp? 1)
(aexp? 3)
(aexp? '(1 + 3))
(aexp? (list 1 '+ '(3 x 4)))
(aexp? 'cookie)
(aexp? (list '(3 expt y) '+ 5))
(quote a)
(quote +)
(quote x)
(eq? (quote a) 'a)

(aexp? '(n + 3))
(numbered? 1)
(numbered? (list 3 '+ (list 4 'expt 5)))
(numbered? '(2 x sausage))

(value 13)
(value '(1 + 3))
(value (list 1 '+ '(3 expt 4)))
(value 'cookie)

(lat?2 (list (edd1 '())
             (edd1 (edd1 '()))
             (edd1 (edd1 (edd1 '())))))

(o+2 (edd1 '())
     (edd1 (edd1 '())))

(zub1 (o+2 (edd1 '())
           (edd1 (edd1 '()))))

(sero? (zub1 (edd1 '())))