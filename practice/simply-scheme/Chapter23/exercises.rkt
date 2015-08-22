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

#! 23.4
(define (vector->list-mod vec)
  (vector->list-helper vec 0))

(define (vector->list-helper vec index)
  (if (>= index (vector-length vec))
      '()
      (cons (vector-ref vec index) (vector->list-helper vec (+ index 1)))))

(vector->list-mod #(1 2 3))

#! 23.5
(define (vector-map fn vec)
  (let ((new (make-vector (vector-length vec))))
    (vector-map-helper fn vec new 0)))

(define (vector-map-helper fn vec new index)
  (if (>= index (vector-length vec))
      new
      (begin (vector-set! new index (fn (vector-ref vec index)))
             (vector-map-helper fn vec new (+ index 1)))))

(vector-map (lambda (x) (* x x)) #(1 2 3 4 5))

#! 23.6
(define (vector-map! fn vec)
  (vector-map!-helper fn vec 0))

(define (vector-map!-helper fn vec index)
  (if (>= index (vector-length vec))
      vec
      (begin (vector-set! vec index (fn (vector-ref vec index)))
             (vector-map!-helper fn vec (+ index 1)))))

(define vector-1 (make-vector 3))
(vector-set! vector-1 0 1)
(vector-set! vector-1 1 2)
(vector-set! vector-1 2 3)
vector-1
(vector-map! (lambda (x) (* x x)) vector-1)

#! 23.7
#|
vector-filter can be written because the lack of an "!" implies just creating a new
vector of only the elements that pass.
It might be easier to use a list as an intermediate data structure, because you don't
know initially how many elements will pass the predicate function and make-vector requires
that you know this. It would be inefficient to use make-vector in each helper call
to construct the new vector.
TODO: is there a way to make it more efficient than just create list then dump list to
a vector?

vector-filter! can't be written because the vector's size would not be able to change.
The creates a problem in taking out elements where the predicate fails. You could do
something like null or zero the elements out, but that doesn't help if you want to
compose just the passing results in a calculation.
|#

(define (vector-filter pred vec)
  (vector-filter-helper pred vec (- (vector-length vec) 1) '()))

(define (vector-filter-helper pred vec index lst)
  (if (< index 0)
      (list->vector-mod lst)
      (if (pred (vector-ref vec index))
          (vector-filter-helper pred vec (- index 1) (cons (vector-ref vec index) lst))
          (vector-filter-helper pred vec (- index 1) lst))))

(define (list->vector-mod lst)
  (let ((vec (make-vector (length lst))))
    (list->vector-helper vec 0 lst)))

(define (list->vector-helper vec index lst)
  (if (null? lst)
      vec
      (begin (vector-set! vec index (car lst))
             (list->vector-helper vec (+ index 1) (cdr lst)))))

(trace list->vector-helper)
(vector-filter (lambda (x) (odd? x)) vector-1)

#! 23.8
(define lap-count-to-win 200)

(define (lap-mod car-number)
  (begin
    (vector-set! *lap-vector*
                 car-number
                 (+ (vector-ref *lap-vector* car-number) 1))
    (if (>= (vector-ref *lap-vector* car-number) lap-count-to-win)
        (show (se "Car" car-number "wins!"))
        void))
  (vector-ref *lap-vector* car-number))

(define (range start stop)
  (range-helper start stop '()))

(define (range-helper index stop lst)
  (if (> index stop)
      lst
      (cons index (range-helper (+ index 1) stop lst))))

(for-each (lambda (x) (lap-mod 34)) (range 1 200))

#! 23.9
(define (leader)
  (leader-helper 0 0))

(define (leader-helper leader index)
  (if (>= index (vector-length *lap-vector*))
      leader
      (let ((leader-laps (vector-ref *lap-vector* leader))
            (current-laps (vector-ref *lap-vector* index)))
        (if (> current-laps leader-laps)
            (leader-helper index (+ index 1))
            (leader-helper leader (+ index 1))))))

(leader)

#! 23.10
(define (leader-23.10)
  (leader-helper-23.10 0 1))

(define (leader-helper-23.10 leader index)
  (cond ((= index 100) leader)
        ((> (lap index) (lap leader))
         (leader-helper index (+ index 1)))
        (else (leader-helper leader (+ index 1)))))
#|
In the second condition, the lap function is being called,
which is incrementing both the leader and the current index,
not just comparing the values that are in *lap-vector*. This
changing state means that leader is not functional, it does not
do the job we want, which is to just report on which index has
the most laps completed.
|#

#! 23.11
(define *menu* '((potstickers 6.00)
                 (wor-won-ton 2.00)
                 (egg-rolls 2.75)
                 (shin-shin-special-prawns 5.85)))

(define *tables* (make-vector 5 0))

(define (order table-num menu-item)
  (let ((menu-item (assoc menu-item *menu*)))
    (if menu-item
        (let ((menu-item-price (cadr menu-item)))
          (vector-set! *tables* (- table-num 1) (+ menu-item-price
                                                   (vector-ref *tables* (- table-num 1)))))
        void)))

(define (bill table-num)
  (let ((the-bill (vector-ref *tables* (- table-num 1)))) ;; Maintain a ref to the bill
    (vector-set! *tables* (- table-num 1) 0) ;; Clear the bill
    the-bill))

(order 3 'potstickers)
(order 3 'not-real);; this should not cause the bill to change...
(order 3 'wor-won-ton)
(order 5 'egg-rolls)
(order 3 'shin-shin-special-prawns)

(bill 3)
(bill 5)

#! 23.12
(define sort-vector (make-vector 6))
(vector-set! sort-vector 0 23)
(vector-set! sort-vector 1 4)
(vector-set! sort-vector 2 18)
(vector-set! sort-vector 3 7)
(vector-set! sort-vector 4 95)
(vector-set! sort-vector 5 60)

(define (selection-sort! vec)
  (selection-sort-helper vec 0))

(define (selection-sort-helper vec index)
  (let ((swap-index (get-swap-index vec index index)))
    (vector-swap! vec index swap-index)
    (if (>= (+ index 1) (vector-length vec))
        vec
        (selection-sort-helper vec (+ index 1)))))

(define (get-swap-index vec current-index swap-index)
  (if (>= current-index (vector-length vec))
      swap-index
      (let ((current-value (vector-ref vec current-index))
            (to-swap-value (vector-ref vec swap-index)))
        (if (< to-swap-value current-value)
            (get-swap-index vec (+ current-index 1) swap-index)
            (get-swap-index vec (+ current-index 1) current-index)))))

(trace selection-sort-helper)
(selection-sort! sort-vector)

#! 23.13
(define (vector-swap2! vector index1 index2)
  (vector-set! vector index1 (vector-ref vector index2))
  (vector-set! vector index2 (vector-ref vector index1)))

#|
The first vector-set! will change index1's value, without saving a reference
to what it was. This means that the value of index2 will end up in both places,
which is not the intent of vector-swap.
|#

#! 23.14
#|
(matrix: two-dimensional version of a vector.)
|#
(define (make-matrix x-dim y-dim)
  (let ((base-vector (make-vector x-dim)))
    (for-each (lambda (index)
                (vector-set! base-vector index (make-vector y-dim)))
              (range 0 (- x-dim 1)))
    base-vector))

(define (matrix-set! matrix x-index y-index value)
  (let ((x-vector (vector-ref matrix x-index)))
    (vector-set! x-vector y-index value)))

(define (matrix-ref matrix x-index y-index)
  (let ((x-vector (vector-ref matrix x-index)))
    (vector-ref x-vector y-index)))

(define m (make-matrix 3 5))

(matrix-set! m 2 1 '(her majesty))

(matrix-ref m 2 1)

#! 23.15

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main array interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-array dimension-list)
  (let ((base-vector (make-vector (car dimension-list))))
    (make-array-helper (cdr dimension-list) base-vector base-vector)))

(define (array-set! array index-list value)
  (array-set-helper array index-list value))

(define (array-ref array index-list)
  (array-ref-helper array index-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-array-helper dimension-list base-vector current-vector)
  (if (null? dimension-list)
      base-vector
      (begin
        (make-vector-for-each-index (car dimension-list) current-vector)
        (vector-for-each-index (lambda (index)
                                 (make-array-helper
                                  (cdr dimension-list)
                                  base-vector
                                  (vector-ref current-vector index)))
                               current-vector)
        base-vector)))

(define (vector-for-each-index fn vec)
  (for-each fn (range 0 (- (vector-length vec) 1))))

(define (make-vector-for-each-index num-items vec)
  (vector-for-each-index (lambda (index)
                           (vector-set! vec index (make-vector num-items)))
                         vec))

(define (array-set-helper vec index-list value)
  (if (null? (cdr index-list))
      (vector-set! vec (car index-list) value)
      (array-set-helper (vector-ref vec (car index-list)) (cdr index-list) value)))

(define (array-ref-helper vec index-list)
  (if (null? (cdr index-list))
      (vector-ref vec (car index-list))
      (array-ref-helper (vector-ref vec (car index-list)) (cdr index-list))))

;;(trace make-array-helper)
(define a0 (make-array '(1 2 3)))
(vector-ref a0 0)
(vector-ref (vector-ref a0 0) 1)
(vector-ref (vector-ref (vector-ref a0 0) 1) 2)

;;(trace array-set-helper)
(array-set! a0 '(0 0 0) 'x0-y0-z0)
(array-set! a0 '(0 0 1) 'x0-y0-z1)
(array-set! a0 '(0 0 2) 'x0-y0-z2)
(array-set! a0 '(0 1 0) 'x0-y1-z0)
(array-set! a0 '(0 1 1) 'x0-y1-z1)
(array-set! a0 '(0 1 2) 'x0-y1-z2)
(show a0)

(define a1 (make-array '(4 5 6)))
(array-set! a1 '(3 2 3) '(the end))
(show a1) ;; Not as friendly...

#! 23.16
#! (a)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main sentence interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (vec-sentence-first-try . rest)
  (let ((num-items (length rest)))
    (let ((vec (make-vector num-items)))
      (vec-sentence-helper vec rest 0)
      vec)))

(define (vec-sentence . lst)
  (let ((vecs (map (lambda (x) (ensure-vector x)) lst)))
    (accumulate (lambda (x y) (vec-concatenate x y)) vecs)))

(define (vec-empty? vec)
  (= (vector-length vec) 0))

(define (vec-first vec)
  (vector-ref vec 0))

(define (vec-butfirst from-vector)
  (one-less-vector-filler from-vector 1))

(define (vec-last vec)
  (let ((last-index (- (vector-length vec) 1)))
    (vector-ref vec last-index)))

(define (vec-butlast from-vector)
  (one-less-vector-filler from-vector 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (vec-sentence-helper vec rest index)
  (if (null? rest)
      vec
      (begin
        (vector-set! vec index (car rest))
        (vec-sentence-helper vec (cdr rest) (+ index 1)))))

;; TODO: May want to play with this for more general purpose.
(define (one-less-vector-filler from-vector from-index)
  (let ((num-items (vector-length from-vector)))
    (let ((to-vector (make-vector (- num-items 1)))
          (num-to-copy (- num-items 1)))
      (vec-build-helper from-vector from-index to-vector 0 num-to-copy))))

(define (vec-build-helper from-vector from-index to-vector to-index remaining)
  (if (= remaining 0)
      to-vector
      (begin
        (vector-set! to-vector to-index (vector-ref from-vector from-index))
        (vec-build-helper from-vector (+ from-index 1) to-vector (+ to-index 1) (- remaining 1)))))

(define (vec-concatenate vec-a vec-b)
  (let ((new (make-vector (+ (vector-length vec-a) (vector-length vec-b)))))
    (vec-filler vec-a 0 new 0 (vector-length vec-a))
    (vec-filler vec-b 0 new (vector-length vec-a) (vector-length vec-b))))

(define (vec-filler from-vector from-index to-vector to-index remaining)
  (if (= remaining 0)
      to-vector
      (begin
        (vector-set! to-vector to-index (vector-ref from-vector from-index))
        (vec-filler from-vector (+ from-index 1) to-vector (+ to-index 1) (- remaining 1)))))

(define (ensure-vector maybe-vec)
  (cond ((vector? maybe-vec) maybe-vec)
        ((list? maybe-vec) (list->vector maybe-vec))
        (else (make-vector 1 maybe-vec))))

(ensure-vector 'a)
(ensure-vector '(1 2 3))
(ensure-vector #(1 2 3))
(vec-concatenate (ensure-vector 'a) (ensure-vector '(1 2)))
(vec-sentence 1 2 3)
(vec-sentence 'a 'b 'c #(1 2 3) '(4 5 6))

(vec-empty? #())
(vec-empty? #(1))

(vec-first #(1 2 3))
(vec-last #(1 2 3))
(vec-sentence 'a 'b 'c)
(vec-butfirst (vec-sentence 'a 'b 'c))
(vec-butlast (vec-sentence 'a 'b 'c))

#! (b)
(define (praise stuff)
  (vec-sentence stuff '(is good)))

(praise 'music)
(praise '(a b c))

(define (vec-praise stuff)
  (vec-sentence stuff #(is good)))

(vec-praise (vec-sentence 'music))
(vec-praise (vec-sentence 'a 'b 'c))

#! (c)
(define (vec-praise-2 stuff)
  (vec-sentence stuff #(rules!)))
(vec-praise-2 (vec-sentence 'music))
(vec-praise-2 (vec-sentence 'a 'b 'c))

#! (d)
(define (vec-item n vec)
  (if (= n 1)
      (vec-first vec)
      (vec-item (- n 1) (vec-butfirst vec))))

(vec-item 1 (vec-sentence 'a 'b 'c))
(vec-item 2 (vec-sentence 'a 'b 'c))
(vec-item 3 (vec-sentence 'a 'b 'c))

;; It is more efficient to use vector-ref to access the item directly.
(define (vec-item-2 n vec)
  (vector-ref vec (- n 1)))

(vec-item-2 1 (vec-sentence 'a 'b 'c))
(vec-item-2 2 (vec-sentence 'a 'b 'c))
(vec-item-2 3 (vec-sentence 'a 'b 'c))

#! (e)
(define (vec-every fn vec)
  (if (vec-empty? vec)
      vec
      (vec-sentence (fn (vec-first vec))
                    (vec-every fn (vec-butfirst vec)))))
#|
 Looking at how vec-every is defined...

 vec-sentence includes calling vec-concatenate,
 which calls make-vector for each pair discovered in the list of arguments.

 vec-first remains constant time, but vec-butfirst also has to create and fill a new vector,
 and is called n-1 times.

 It can be done better by making the vector once, and using vector-set!, which is a
 constant time operation.
|#
(define (vec-every-2 fn vec)
  (let ((new (make-vector (vector-length vec))))
    (vec-every-helper fn vec new (- (vector-length vec) 1))))

(define (vec-every-helper fn vec new index)
  (if (< index 0)
      new
      (begin
        (vector-set! new index (fn (vector-ref vec index)))
        (vec-every-helper fn vec new (- index 1)))))

;;(trace vec-sentence)
;;(trace vec-filler)
(vec-every (lambda (x) (* x 2)) #(1 2 3))
(vec-every-2 (lambda (x) (* x 2)) #(1 2 3))

#! (f)
#|
 The constant time operations for vector: vector-ref, vector-set!, vector-length
 The constant time operations for list: cons, car, cdr, null?

 selectors:
 {vector-ref, vector-set!}
 {list-ref, car, cdr}

 constructors:
 {make-vector}
 {cons}

 Using vectors to implement sentences makes the selectors fast, but the constructor is
 slow.

 I assume lists were used because the desire was to construct and perform operations on
 whole sentences, not to find single words in them. This would mean cons, car, and cdr
 being fast is sufficient, which are the constant time operations for lists.
|#
