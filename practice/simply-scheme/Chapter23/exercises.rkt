#lang racket
(require (planet dyoo/simply-scheme:2))
(require racket/trace)

(define v (make-vector 5))
(vector-set! v 0 'shoe)
(vector-set! v 3 'bread)
(vector-set! v 2 '(savoy truffle))
(vector-ref v 3)
(vector-set! v 3 'jewel)
(vector-ref v 3)
(vector-set! v 1 741)
(vector-set! v 4 #t)

v

(define *lap-vector* (make-vector 100))

(define (initialize-lap-vector index)
  (if (< index 0)
      'done
      (begin (vector-set! *lap-vector* index 0)
             (initialize-lap-vector (- index 1)))))

(initialize-lap-vector 99)

(define (lap car-number)
  (vector-set! *lap-vector*
               car-number
               (+ (vector-ref *lap-vector* car-number) 1))
  (vector-ref *lap-vector* car-number))

(lap 1)
(lap 2)
(lap 1)