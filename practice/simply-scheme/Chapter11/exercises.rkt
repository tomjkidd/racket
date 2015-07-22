#lang racket
(require (planet dyoo/simply-scheme:2))

#! Start by thinking of a simple procedure for each length
(define (downup1 wd)
  (se wd))

(downup1 'a)

(define (downup2 wd)
  (se wd (first wd) wd))

(downup2 'be)

(define (downup3 wd)
  (se wd
      (bl wd)
      (first wd)
      (bl wd)
      wd))

(downup3 'foo)

(define (downup3-mod wd)
  (se wd (downup2 (bl wd)) wd))

(downup3-mod 'foo)

(define (downup4 wd)
  (se wd (downup3-mod (bl wd)) wd))

(downup4 'paul)

(define (downup wd)
  (if (= (count wd) 1)
      (se wd)
      (se wd (downup (bl wd)) wd)))

(downup 'toe)
(downup 'banana)
(downup 'pneumonoultramicroscopicsilicovolcanoconinosis)

(define (pigl0 wd)
  (word wd 'ay))

(pigl0 'alabaster)

(define (pigl1 wd)
  (word (bf wd) (first wd) 'ay))

(pigl1 'salami)

(define (pigl wd)
  (if (member? (first wd) 'aeiou)
      (word wd 'ay)
      (pigl (word (bf wd) (first wd)))))

(pigl 'scheme)

(define (explode wd)
  (if (= (count wd) 1)
      wd
      (se
       (first wd)
       (explode (bf wd)))))

(explode 'dynamite)

#! NOTE: The book used a method where 0 length wds can come it, that is more robust, but I wanted to keep my definitions.
(define (letter-pairs wd)
  (if (<= (count wd) 2)
      wd
      (se
       (word (first wd) (first (bf wd)))
       (letter-pairs (bf wd)))))

(letter-pairs 'george)

#! 11.1
(define (downup4-mod wd)
  (se
   wd
   (bl wd)
   (bl (bl wd))
   (bl (bl (bl wd)))
   (bl (bl wd))
   (bl wd)
   wd))
(downup4-mod 'four)

#! 11.2
(define (count-ums sent)
  (if (= (count sent) 0)
      0
      (+ (if (equal? 'um (first sent)) 1 0)
         (count-ums (bf sent)))))
(count-ums '(today um we are going to um talk about the combining um method))

#! 11.3
;; NOTE: From Chapter 8
(define (unspell-letter letter)
  (cond ((member? letter '(a b c)) 2)
        ((member? letter '(d e f)) 3)
        ((member? letter '(g h i)) 4)
        ((member? letter '(j k l)) 5)
        ((member? letter '(m n o)) 6)
        ((member? letter '(p q r s)) 7)
        ((member? letter '(t u v)) 8)
        ((member? letter '(w x y z)) 9)))

(define (phone-unspell wd)
  (if (= (count wd) 0)
      ""
      (word (unspell-letter (first wd)) (phone-unspell (bf wd)))))

(phone-unspell 'popcorn)