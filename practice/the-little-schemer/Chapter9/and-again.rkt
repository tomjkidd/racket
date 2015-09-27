#lang racket

(require "../full-core.rkt")

(looking 'caviar '(6 2 4 caviar 5 7 3))

(pick 1 '(6 2 4 caviar 5 7 3))

(looking 'caviar '(6 2 grits caviar 5 7 3))

;; (looking 'caviar '(7 1 2 caviar 5 6 3)) ;; Goes into an infinite loop
#|
looking asks for 1 -> 7 -> 3 -> 2 -> 1 -> 7, which then repeats over an over.
|#
(shift (list '(a b) 'c))

(shift (list '(a b) '(c d)))

(weight* (list '(a b) 'c))
(weight* (list 'a '(b c)))

(shuffle (list 'a '(b c)))
(shuffle '(a b))
;; (shuffle (list '(a b) '(c d))) ;; Goes into an infinite loop

(A 1 2)
;; (A 4 3) ;; Does not terminate.
