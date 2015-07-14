#lang racket
(require (planet dyoo/simply-scheme))

(define (acronym phrase)
  (accumulate word (every first phrase)))

(define (acronym-2 phrase)
  (accumulate word (every first (keep real-word? phrase))))
(define (real-word? wd)
  (not (member? wd '(a the an in of and for to with))))

(define (pigl wd)
  (if (member? (first wd) 'aeiou)
      (word wd 'ay)
      (pigl (word (butfirst wd) (first wd)))))

(define (rotate wd)
  (word (butfirst wd) (first wd)))

#! every will call pigl on each word
(every pigl '(the ballad of john and yoko))