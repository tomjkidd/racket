#lang racket

(require (planet dyoo/simply-scheme:2))
(require racket/trace)

(provide for-each-with-index)

(define (for-each-with-index fn lst)
  (for-each-with-index-helper lst fn 0))

(define (for-each-with-index-helper lst fn index)
  (if (null? lst)
      void
      (begin
        (fn (car lst) index)
        (for-each-with-index-helper (cdr lst) fn (+ index 1)))))
