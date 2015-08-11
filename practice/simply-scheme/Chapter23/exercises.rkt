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

(define (card-list)
  (reduce append
          (map (lambda (suit) (map (lambda (rank) (word suit rank))
                                   '(a 2 3 4 5 6 7 8 9 10 j q k)))
               '(h s d c))))

(card-list)

(define (make-deck)
  (shuffle! (list->vector (card-list)) 51))

(define (shuffle! deck index)
  (if (< index 0)
      deck
      (begin (vector-swap! deck index (random (+ index 1)))
             (shuffle! deck (- index 1)))))

(define (vector-swap! vector index1 index2)
  (let ((temp (vector-ref vector index1)))
    (vector-set! vector index1 (vector-ref vector index2))
    (vector-set! vector index2 temp)))

(make-deck)
(make-deck)

(define (square x) (* x x))

(define (list-square numbers)
  (if (null? numbers)
      '()
      (cons (square (car numbers))
            (list-square (cdr numbers)))))

(define (vector-square numbers)
  (vec-sq-helper (make-vector (vector-length numbers))
                 numbers
                 (- (vector-length numbers) 1)))

(define (vec-sq-helper new old index)
  (if (< index 0)
      new
      (begin (vector-set! new index (square (vector-ref old index)))
             (vec-sq-helper new old (- index 1)))))

(list-square '(1 2 3 4 5))
(vector-square #(1 2 3 4 5))

(define dessert (vector 'chocolate 'sundae))
(define two-desserts (list dessert dessert))
(vector-set! (car two-desserts) 1 'shake)
two-desserts

(define two-desserts2 (list (vector 'chocolate 'sundae)
                            (vector 'chocolate 'sundae)))
(vector-set! (car two-desserts2) 1 'shake)
two-desserts2

#! 23.1
(define (sum-vector vec)
  (sum-vector-helper vec 0 0))

(define (sum-vector-helper vec index sum)
  (if (>= index (vector-length vec))
      sum
      (sum-vector-helper vec
                         (+ index 1)
                         (+ (vector-ref vec index) sum))))

(sum-vector #(6 7 8))

#! 23.2
(define (vector-fill-mod! vec value)
  (vector-fill-helper vec value 0))

(define (vector-fill-helper vec value index)
  (if (>= index (vector-length vec))
      vec
      (begin (vector-set! vec index value)
             (vector-fill-helper vec value (+ index 1)))))
          

(define vec (vector 'one 'two 'three 'four))
vec
(vector-fill-mod! vec 'yeah)
vec

#! 23.3
(define (vector-append vec1 vec2)
  (let ((new (make-vector (+ (vector-length vec1) (vector-length vec2)))))
    (vector-append-helper vec1 vec2 new 0)))

(define (vector-append-helper vec1 vec2 comb index)
  (cond ((>= index (vector-length comb)) comb)
        ((>= index (vector-length vec1)) (let ((index-in-2 (- index (vector-length vec1))))
                                           (begin (vector-set! comb
                                                               index
                                                               (vector-ref vec2 index-in-2))
                                                  (vector-append-helper vec1 vec2 comb (+ index 1)))))
        (else (begin (vector-set! comb
                                  index
                                  (vector-ref vec1 index))
                     (vector-append-helper vec1 vec2 comb (+ index 1))))))

(vector-append '#(not a) '#(second time))