#lang racket
(require (planet dyoo/simply-scheme:2))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (average a b)
  (/ (+ a b) 2))

(define (hypotenuse a b)
  (sqrt (+ (square a) (square b))))

#! 4.4
(define (sphere-volume r)
  (* (/ 4 3) 3.141592654 (cube r)))

(define (next x)
  (+ x 1))

(define (triangle-area b h)
  (* 0.5 b h))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

#! 4.5
(define (c-to-f c)
  (+ (* (/ 9 5) c) 32))
(define (f-to-c f)
  (* (/ 5 9) (- f 32)))

(c-to-f 0)
(c-to-f 25)
(c-to-f 100)
(f-to-c 77)
(f-to-c 80)

#! 4.6
(define (mult-fourth x)
  (* x x x x))
(define (fourth x)
  (square (square x)))

#! 4.7
(define (absolute x)
  (sqrt (square x)))
(absolute -1)

#! 4.8
(define (scientific n exp)
  (* n (expt 10 exp)))
(scientific 7 3)
(scientific 42.0 -5.0)

#! x = c*(10^e) -> x/(10^y) = c
(define (sci-coefficient x)
  (/ x (expt 10 (sci-exponent x))))

#! logBase10(x) = logBaseY(x)/logBaseY(10)
(define (sci-exponent x)
  (floor (/ (log x) (log 10))))

(sci-coefficient 7000)
(sci-exponent 7000)

#! 4.9
(define (discount init percent)
  (* init (- 1 (/ percent 100))))
(discount 10 5)
(discount 29.90 50)

#! 4.10
(define (round-cents bill)
  (/ (ceiling (* 100 bill)) 100)) ;; Convert to pennies, ceiling, then back to round
(define (bill-and-tip bill)
  (ceiling (+ bill (just-tip bill)))) ;; bill-and-tip is the total that will be paid.
(define (just-tip bill)
  (* bill 0.15)) ;; The raw tip is 15%
(define (tip bill)
  (round-cents (- (bill-and-tip bill) bill))) ;; total bill - orig bill is total tip

(tip 19.98)
(tip 29.23)
(tip 7.54)