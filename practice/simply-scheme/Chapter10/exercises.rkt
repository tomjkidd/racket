#lang racket
(require (planet dyoo/simply-scheme:2))

#| First version
(define (ttt position me)
  (cond ((i-can-win?) (choose-winning-move))
        ((opponent-can-win?) (block-opponent-win))
        ((i-can-win-next-time?) (prepare-win))
        (else (whatever))))
|#

(define (find-triples position)
  (every (lambda (comb) (substitue-triple comb position))
         '(123 456 789 147 258 369 159 357)))

(define (substitue-triple combination position)
  (accumulate word
              (every (lambda (square)
                       (substitute-letter square position))
                       combination)))

(define (substitute-letter square position)
  (if (equal? '_ (item square position))
      square
      (item square position)))

(substitue-triple 456 '_xo_x_o__)
(substitue-triple 147 '_xo_x_o__)
(substitue-triple 357 '_xo_x_o__)

(find-triples '_xo_x_o__)
(find-triples 'x_____oxo)
