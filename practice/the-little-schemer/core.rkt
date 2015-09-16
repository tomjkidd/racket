#lang racket

(provide atom? s-expr? count-s-expr)

;; An atom is not a pair and not null (the empty list)
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define s-expr?
  (lambda (x)
    (or (atom? x)
        (list? x))))

(define (count-s-expr lst)
  (count-s-expr-helper lst 0))

(define (count-s-expr-helper lst count)
  (if (null? lst)
      count
      (let ((to-add (if (s-expr? (car lst))
                        1
                        0)))
        (count-s-expr-helper (cdr lst) (+ count to-add)))))
