#lang racket
(require (planet dyoo/simply-scheme:2))

(define (square x) (* x x))

#! 6.1
#! First true expression will return it's expression
#! (= 3 4) is #f, (< 2 3) is #t, '(nowhere man)
(cond ((= 3 4) '(this boy))
      ((< 2 5) '(nowhere man))
      (else '(two of us)))
#! This one tricked me, 3 is what it evaluates to.
#! Has to do with each condition containing two expressions, empty? is an expression (function) that is 'true',
#! so the second expression, 3, is evaluated.
(cond (empty? 3)
      (square 7)
      (else 9))

#! 6.2
(or #f #f #f #t) ;; #t
(and #f #f #f #t) ;; #f
(or (= 2 3) (= 4 3)) ;; #f
(not #f) ;; #t
(or (not (= 2 3)) (= 4 3)) ;; #t
(or (and (= 2 3) (= 3 3)) (and (< 2 3) (< 3 4))) ;; #t

#! 6.3
(define (sign number)
  (cond ((< number 0) 'negative)
        ((= number 0) 'zero)
        (else 'positive)))

(sign -1)
(sign 0)
(sign 1)

#! 6.4
(define (utensil meal)
  (if (equal? meal 'chinese) 'chopsticks 'fork))

(utensil 'chinese)
(utensil 'indian)

#! 6.5
#|
 1-11, same with am
 ...
 12 -> PM
 13 -> 1PM, -12 with pm
 ...
 24 -> 12 AM
|#
(define (european-time time-am-pm)
  (define hour (first time-am-pm))
  (define am-pm (first (bf time-am-pm)))
  (cond ((equal? am-pm 'am) (if (= hour 12) 24 hour))
        (else (+ hour 12))))

(define (american-time hour)
  (cond ((= hour 24) (sentence 12 'am))
        ((< hour 12) (sentence hour 'am))
        ((= hour 12) (sentence hour 'pm))
        (else (sentence (- hour 12) 'pm))))

(european-time '(8 am))
(european-time '(4 pm))
(american-time 21)
(american-time 12)
(european-time '(12 am))
