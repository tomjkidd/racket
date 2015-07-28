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

#! 17.4
(define (mystery lst)
  (mystery-helper lst '()))

(define (mystery-helper lst other)
  (if (null? lst)
      other
      (mystery-helper (cdr lst) (cons (car lst) other))))

(mystery '(a b c d e f)) ;; '(f e d c b a), reverses the list

#! 17.5
(define (max2 a b)
  (if (> b a) b a))

(define (max-mod number . rest)
  (cond ((null? rest) number)
        (else (apply max-mod (cons (max2 number (car rest))
                                   (cdr rest))))))

(max-mod 1 2 5 4 3)

#! 17.6
(define reverse-mod mystery)

(define (append-helper lst-a lst-b)
  (cond ((null? lst-a) lst-b)
        (else (append-helper (cdr lst-a) (cons (car lst-a) lst-b)))))

(define (append-2 lst-a lst-b)
  (append-helper (reverse lst-a) lst-b))

(reverse '(a b c))

(trace append-helper)

(append-2 '(a b c) '(d e f))

#! 17.7
(define (sentence-2 a b)
  (append (if (word? a)
              (list a)
              a)
          (if (word? b)
              (list b)
              b)))

(define (sentence-mod a . rest)
  (append (if (word? a)
              (list a)
              a)
          (cond ((null? rest) '())
                ((word? rest) rest)
                (else (apply sentence-mod rest)))))
;; The existence of apply makes it so that you don't have to work out the details of
;; if rest is null, a word, or list in the recursive case, you can simply pass it
;; forward with another recursive call. Nice.

(sentence-2 '(a b) '(c d))
(sentence-2 'a '(b c d))
(sentence-2 '(a b c) 'd)
(sentence-2 'ab 'cd)

(sentence-mod 'a)
(sentence-mod '(a))
(sentence-mod '(a b) '(c d))
(sentence-mod 'a '(b c d))
(sentence-mod '(a b c) 'd)
(sentence-mod 'ab 'cd)
(sentence-mod '(a b) '(c d) 'e)
(sentence-mod '(a b) '(c d) '(e f))

#! 17.8
(define (member-mod needle haystack)
  (if (null? haystack)
      #f
      (if (equal? needle (car haystack))
          haystack
          (member-mod needle (cdr haystack)))))

(member-mod 'my '(i lost my phone in the pool))
(member-mod 'phone '(i lost the bike over a cliff))

#! 17.9
(define (list-ref-mod lst index)
  (if (= index 0)
      (car lst)
      (list-ref-mod (cdr lst) (- index 1))))

(list-ref-mod '(a b c) 0)
(list-ref-mod '(a b c) 1)
(list-ref-mod '(a b c) 2)

#! 17.10
(define (length-mod lst)
  (if (null? lst)
      0
      (+ 1 (length-mod (cdr lst)))))

(length-mod '(1 2 3))

#! 17.11
(define (before-in-list? lst x y)
  (cond ((null? lst) #f)
        ((equal? (car lst) x) #t)
        ((equal? (car lst) y) #f)
        (else (before-in-list? (cdr lst) x y))))

(before-in-list? '(back in the ussr) 'in 'ussr)
(before-in-list? '(back in the ussr) 'the 'back)
(before-in-list? '(back in the ussr) 'butter 'frisbee)

#! 17.12
(define (flatten structure)
  (cond ((null? structure) '())
        ((word? structure) structure)
        ((list? structure) (se (flatten (car structure))
                                   (flatten (cdr structure))))))
;;(trace flatten)

(flatten '(((a b) c (d e)) (f g) ((((h))) (i j) k)))

#! 17.13
(define (deep-count lst)
  (cond ((null? lst) 0)
        ((word? (car lst)) (+ 1 (deep-count (cdr lst))))
        (else (+ (deep-count (car lst))
                 (deep-count (cdr lst))))))

(define (deep-count-simpler lst)
  (cond ((null? lst) 0)
        ((word? lst) 1)
        (else (+ (deep-count-simpler (car lst))
                 (deep-count-simpler (cdr lst))))))

(deep-count '(((a b) c (d e)) (f g) ((((h))) (i j) k)))
(deep-count-simpler '(((a b) c (d e)) (f g) ((((h))) (i j) k)))

#! 17.14
(define (branch lst-num structure)
  (cond ((null? lst-num) structure)
        ((list? lst-num) (branch (cdr lst-num)
                                 (list-ref structure (- (car lst-num) 1))))))
;;(trace branch)
(branch '(3) '((a b) (c d) (e f) (g h)))
(branch '(3 2) '((a b) (c d) (e f) (g h)))
(branch '(2 3 1 2) '((a b) ((c d) (e f) ((g h) (i j)) k) (l m)))