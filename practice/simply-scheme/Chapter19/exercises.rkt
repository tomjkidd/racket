#lang racket
(require (planet dyoo/simply-scheme:2))
(require racket/trace)

(define (area shape r) (* shape r r))
(define square 1)
(define circle pi)
(define sphere (* 4 pi))
(define hexagon (* (sqrt 3) 1.5))

(area sphere 7)

(define (every-mod fn sent)
  (if (empty? sent)
      '()
      (se (fn (first sent))
          (every-mod fn (bf sent)))))

(define (map-mod fn lst)
  (if (null? lst)
      '()
      (cons (fn (car lst))
            (map-mod fn (cdr lst)))))

(map-mod (lambda (flavor) (se flavor '(is great)))
     '(ginger (ultra chocolate) pumpkin (rum raisin)))

(define (filter-mod pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) (cons (car lst) (filter-mod pred (cdr lst))))
        (else (filter-mod pred (cdr lst)))))

(filter-mod (lambda (x) (number? x)) '(a b c 1 2 3 d e f 4 5 6))

(define (deep-map f structure)
  (cond ((word? structure) (f structure))
        ((null? structure) '())
        (else (cons (deep-map f (car structure))
                    (deep-map f (cdr structure))))))

(define (pigl-helper index wd)
  (if (>= index (count wd))
      (word wd 'ay)
      (if (member? (first wd) 'aeiou)
          (word wd 'ay)
          (pigl-helper (+ index 1) (word (bf wd) (first wd))))))
      
(define (pigl wd)
  (pigl-helper 0 wd))

(deep-map pigl (make-node 'funny
                          (list (make-node 'bones '())
                                (make-node 'cold '())
                                (make-node 'orbit '()))))

#! 19.1
(every cdr '((john lennon) (paul mccartney) (george harrison) (ringo starr))) ;; '(lennon mccartney harrison starr), creates a sentence.

#! 19.2
(define (keep-mod pred sent)
  (cond ((null? sent) sent)
        ((empty? sent) sent)
        ((word? sent) (if (pred (first sent))
                          (word (first sent)
                              (keep-mod pred (bf sent)))
                          (keep-mod pred (bf sent))))
        ((list? sent) (if (pred (car sent))
                          (cons (car sent)
                                (keep-mod pred (cdr sent)))
                          (keep-mod pred (cdr sent))))
        (else (error "Can't perform keep"))))

(keep number? 'a1b2c3)
(keep number? '(a 1 b 2 c 3))
(keep number? "")
(keep number? '())

(keep-mod number? 'a1b2c3)
(keep-mod number? '(a 1 b 2 c 3))
(keep-mod number? "")
(keep-mod number? '())

#! 19.3
(define (three-arg-accumulate combiner accum lst)
  (cond ((null? lst) accum)
        ((list? lst)
         (combiner (car lst)
                   (three-arg-accumulate combiner accum (cdr lst))))))

(define (three-arg-accumulate-reverse combiner accum lst)
  (cond ((null? lst) accum)
        ((list? lst)
         (three-arg-accumulate-reverse combiner
                               (combiner (car lst) accum)
                               (cdr lst)))))
;; Note by reversing the application of three-arg-accumulate and the combiner
;; the order that the elements are consumed in is reversed.

(three-arg-accumulate + 0 '(4 5 6))
(three-arg-accumulate + 0 '())
(three-arg-accumulate cons '() '(a b c d e))
(three-arg-accumulate-reverse cons '() '(a b c d e))

#! 19.4
(define (accumulate-mod combiner stuff)
  (if (empty? (bf stuff))
      (first stuff)
      (combiner (first stuff) (accumulate-mod combiner (bf stuff)))))

(trace accumulate-mod)
(accumulate-mod - '(2 3 4 5))

(define (left-accumulate-helper combiner temp stuff)
  (cond ((empty? stuff) (if (empty? temp)
                            #f
                            (car temp))) ;; Access the value from the accumulator.
        ;; temp is used as a container for the running result.
        ;; When temp is empty, populate it with the head of the list.
        ((empty? temp) (left-accumulate-helper combiner
                                               (cons (first stuff) temp)
                                               (bf stuff)))
        ;; When temp has a value, use combiner with head of the list to get new accumulator.
        ((= (length temp) 1) (left-accumulate-helper combiner
                                                     (list (combiner (first temp) (first stuff)))
                                                     (bf stuff)))))

(define (left-accumulate combiner stuff)
  (left-accumulate-helper combiner '() stuff))

;; Including this because I discovered that it also works.
(define (left-accumulate-har-har combiner stuff)
  (apply combiner stuff))

(trace left-accumulate-helper)
(trace left-accumulate)
(left-accumulate - '(2 3 4 5))

#! 19.5
(define (true-for-all? fn lst)
  (cond ((empty? lst) #t)
        ((fn (car lst)) (true-for-all? fn (cdr lst)))
        (else #f)))

(true-for-all? even? '(2 4 6 8))
(true-for-all? even? '(2 6 3 4))

#! 19.6
(define (true-for-any-pair? pred sent)
  (cond ((empty? (bf sent)) #f)
        (else (if (pred (first sent) (first (bf sent)))
                  #t
                  (true-for-any-pair? pred (bf sent))))))

(true-for-any-pair? equal? '(a b c b a))
(true-for-any-pair? equal? '(a b c c d))
(true-for-any-pair? < '(20 16 5 8 6))
                  
#! 19.7
(define (true-for-all-pairs? pred sent)
  (cond ((empty? (bf sent)) #t)
        (else (and (pred (first sent) (first (bf sent)))
                   (true-for-all-pairs? pred (bf sent))))))
'(tests for 19.7)
(true-for-all-pairs? equal? '(a b c c d))
(true-for-all-pairs? equal? '(a a a a a))
(true-for-all-pairs? < '(20 16 5 8 6))
(true-for-all-pairs? < '(3 7 19 22 43))

#! 19.8
(define (true-for-all-pairs-non-recursive? pred sent)
  ;; By reversing the predicate, we can return if the predicate is false for any pair.
  ;; By reversing the overall answer, we can say it is true if it is not false for any pair.
  (not (true-for-any-pair? (lambda (a b) (not (pred a b))) sent)))

'(tests for 19.8)
(true-for-all-pairs-non-recursive? equal? '(a b c c d))
(true-for-all-pairs-non-recursive? equal? '(a a a a a))
(true-for-all-pairs-non-recursive? < '(20 16 5 8 6))
(true-for-all-pairs-non-recursive? < '(3 7 19 22 43))

#! 19.9
(define (earliest-helper so-far rest sort-by)
  (cond ((empty? rest) so-far)
        ((sort-by so-far (first rest))
         (earliest-helper so-far (bf rest) sort-by))
        (else (earliest-helper (first rest) (bf rest) sort-by))))

(define (earliest-word sent sort-by)
  (earliest-helper (first sent) (bf sent) sort-by))

(define (remove-once wd sent)
  (cond ((empty? sent) '())
	((equal? wd (first sent)) (bf sent))
	(else (se (first sent) (remove-once wd (bf sent))))))

(define (sort-mod sent sort-by)
  (if (empty? sent)
      '()
      (se (earliest-word sent sort-by)
          (sort-mod (remove-once (earliest-word sent sort-by) sent) sort-by))))

(sort-mod '(the whole world loves it when you are in the news) before?)

(sort-mod '(4 23 7 5 16 3) <)
(sort-mod '(4 23 7 5 16 3) >)
(sort-mod '(john paul george ringo) before?)

#! 19.10
(define (tree-map fn tree)
  (make-node (fn (datum tree))
             (map (lambda (child) (tree-map fn child))
                    (children tree))))

(define tree-a (make-node 2 (list (make-node 3 (list (make-node 5 '())))
                                  (make-node 4 (list (make-node 6 '())
                                                     (make-node 7 '()))))))

(define tree-b (make-node 3 (list (make-node 4 '())
                                  (make-node 7 '())
                                  (make-node 2 (list (make-node 3 '())
                                                     (make-node 8 '()))))))

tree-a
(tree-map (lambda (node-val) (* node-val 2)) tree-a)

tree-b
(tree-map (lambda (node-val) (+ node-val 5)) tree-b)

#! 19.11
(define (repeated-mod fn num-times)
  ;; The recursive call will be a function of two args
  ;; The result that we want is a function of a single arg.
  ;; TODO: Write a good explanation for why this works
  (if (= num-times 0)
      (lambda (x) x)
      (lambda (x)
        ((repeated-mod fn (- num-times 1)) (fn x)))))

(define (plus-one x)
  (+ 1 x))

((repeated plus-one 12) 0)
((repeated-mod plus-one 12) 0)

#! 19.12
(define (tree-reduce fn tree)
  (if (empty? (children tree))
      (fn (datum tree))
      (apply fn (cons (datum tree)
                      (map (lambda (child) (tree-reduce fn child))
                           (children tree))))))

(trace tree-reduce)
(tree-reduce + tree-a)
(tree-reduce + tree-b)

#! 19.13
(define (deep-reduce fn structure)
  (cond ((list? structure)
         (apply fn (map (lambda (sublist) (deep-reduce fn sublist))
                        structure)))
        (else (fn structure))))

(deep-reduce word '(r ((a (m b) (l)) (e (r)))))
(deep-reduce + '(1 ((2 (3 4) (5)) (6 (7)))))
