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
