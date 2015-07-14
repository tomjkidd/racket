#lang racket
(require (planet dyoo/simply-scheme))
#|
If wd starts with a vowel, append "ay" to the input
If wd starts with a consonant, move all the starting consonants to the end and append "ay" to the end.
|#
(define (square x) (* x x))
(define (pigl wd) (first wd))

(define (largest-square total guess)
  (define (next-guess guess) (+ guess 1))
  (define (good-enough? total guess)
    (< total (square(next-guess guess))))
  (if (good-enough? total guess)
      guess
      (largest-square total (next-guess guess))))
#! The definition provided has a bug when the guess is larger than the actual answer...