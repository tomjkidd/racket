#lang racket
(require (planet dyoo/simply-scheme:2))

(define (roots a b c)
  (let ((discriminant (sqrt (- (* b b) (* 4 a c))))
        (minus-b (- b))
        (two-a (* 2 a)))
    (se (/ (+ minus-b discriminant) two-a)
        (/ (- minus-b discriminant) two-a))))

(roots 1 2 3)

(define (vowel? letter)
  (member? letter '(a e i o u)))

#! 7.1
(define (gertrude-repetitive wd)
  (se (if (vowel? (first wd)) 'an 'a)
      wd
      'is
      (if (vowel? (first wd)) 'an 'a)
      wd
      'is
      (if (vowel? (first wd)) 'an 'a)
      wd))

(define (gertrude wd)
  (let ((a-or-an (if (vowel? (first wd)) 'an 'a))
        (word-is (se wd 'is)))
    (se a-or-an
        word-is
        a-or-an
        word-is
        a-or-an
        wd )))

(gertrude 'rose)
(gertrude 'iguana)

#! 7.2
(let ((pi 3.14159)
      (pie '(lemon meringue)))
  (se '(pi is) pi '(but pie is) pie))

#! 7.3 Again with hiding a global definition (word).
(define (superlative adjective wd)
  (se (word adjective 'est) wd))

(superlative 'dumb 'exercise)

#! 7.4 Renames and swaps the addition and multiplication operators
(define (sum-square a b)
  (let ((+ *)
        (* +))
    (* (+ a a) (+ b b))))
(sum-square 3 4)