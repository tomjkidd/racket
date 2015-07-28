#lang racket
(require (planet dyoo/simply-scheme:2))
(require racket/trace)

(define (increasing? number . rest-of-numbers)
  (cond ((null? rest-of-numbers) #t)
        ((> (car rest-of-numbers) number)
         (apply increasing? rest-of-numbers))
        (else #f)))

(increasing? 4 12 82)
(increasing? 12 4 82 107)
(increasing? 4)

;; NOTE: rest-of-numbers is the empty list when only 1 arg is used.
;; apply is used like in javascript...
;; It allows you to provide a list of arguments to a function as a single parameter,
;; but spreads the elements in the list as a series of arguments.
(+ 3 4 5)
(apply + '(3 4 5))

(define (deep-appearances wd structure)
  (if (word? structure)
      (if (equal? structure wd) 1 0)
      (reduce +
              (map (lambda (sublist) (deep-appearances wd sublist))
                   structure))))

(deep-appearances 'the '(((the man) in ((the) moon)) ate (the) potstickers))

#! 17.1
(define names '(Rod Chris Colin Hugh Paul))
(car names) ;; 'Rod
(cadr names) ;; 'Chris
(cdr names) ;; '(Chris Colin Hugh Paul)
;;(car 'Rod) ;; Error, contract violation, expected pair?
(cons '(Rod Argent) '(Chris White)) ;; '((Rod Argent) Chris White)
(append '(Rod Argent) '(Chris White)) ;; '(Rod Argent Chris White)
(list '(Rod Argent) '(Chris White)) ;; '((Rod Argent) (Chris White))

(define more-names '((Rod Argent) (Chris White)
                      (Colin Blunstone) (Hugh Grundy) (Paul Atkinson)))
(caadr more-names) ;; 'Chris, (car (car (cdr more-names)))
(assoc 'Colin more-names) ;; '(Colin Blunstone)
(assoc 'Argent more-names) ;; #f

#! 17.2
(define (f1 lst-a lst-b)
  (list (append (cdr lst-a) (list (car lst-b)))))

(f1 '(a b c) '(d e f))

(define (f2 lst-a lst-b)
  (list (cdr lst-a) (cadr lst-b)))

(f2 '(a b c) '(d e f))

(define (f3 lst-a lst-b)
  (append lst-a lst-a))

(f3 '(a b c) '(d e f))

(define (f4 lst-a lst-b)
  (list (list (car lst-a) (car lst-b))
        (append (cdr lst-a) (cdr lst-b))))

(f4 '(a b c) '(d e f))

#! 17.3
(let ((fns (map (lambda (x) (lambda (y) (+ x y))) '(1 2 3 4))))
  (every (lambda (fn) (fn 1)) fns)) ;; '(2 3 4 5)
;; fns is a list of four functions that each take a single argument
;; The first adds one to the argument
;; The second adds two to the argument
;; The third adds three, fourth adds four.