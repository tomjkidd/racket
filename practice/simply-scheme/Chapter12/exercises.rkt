#lang racket
(require (planet dyoo/simply-scheme:2))

#! 12.1
(define (addup nums)
  (if (empty? nums)
      0
      (+ (first nums) (addup (bf nums)))))

(addup '(1 2 3 4 5))

#! 12.2
(define (acronym sent)
  (if (= (count sent) 1)
      (first (first sent)) ;; The error was there
      (word (first (first sent))
            (acronym (bf sent)))))

(acronym '(hold on to your hat))

#! 12.3
#| if you reduce the factorial base case from 0 to -1, the resulting procedure will
pass 0 to a multiplication for n=0, because it no longer is a speacial case. This will
make the product 0, which is wrong. |#
(define (factorial n)
  (if (= n -1)
      1
      (* n (factorial (- n 1)))))

(factorial 3) ;; 0, not right!

#! 12.4
(define (f sent)
  (if (empty? sent)
      sent
      (sentence (f (bf sent)) (first sent))))
(f '(this is a test)) ;; '(test a is this), f reverses the order of words in a sentence

#! 12.5
(define (exaggerate sent)
  (if (empty? sent)
      sent
      (sentence (cond ((number? (first sent)) (* 2 (first sent)))
                      ((equal? (first sent) 'good) 'great)
                      ((equal? (first sent) 'bad) 'terrible)
                      (else (first sent)))
                (exaggerate (bf sent)))))

(exaggerate '(i ate 3 potstickers))
(exaggerate '(the chow fun is good here))
(exaggerate '(the chow fun is bad here))

#! 12.6
(define (base-grade grade)
  (let ((grade-letter (first grade)))
    (cond ((equal? grade-letter 'A) 4)
          ((equal? grade-letter 'B) 3)
          ((equal? grade-letter 'C) 2)
          ((equal? grade-letter 'D) 1)
          (else 0))))

(define (grade-modifier grade)
  (let ((modifier (if (equal? (bf grade) "")
                      ""
                      (first (bf grade)))))
    (cond ((equal? modifier '+) 0.33)
          ((equal? modifier '-) -0.33)
          (else 0))))

(define (one-when-not-empty x)
  (if (equal? x 0)
      0
      1))

(define (grade-sum grades)
  (if (empty? grades)
      0
      (let ((grade (first grades)))
        (+ (base-grade grade)
           (grade-modifier grade)
           (grade-sum (bf grades))))))

(define (gpa grades)
  (/ (grade-sum grades) (count grades)))

(gpa '(A A+ B+ B))
(/ (+ 4 4.33 3.33 3) 4)
(gpa '(A A A))

#! 12.12
(define (roman-value letter)
  (cond ((equal? letter 'i) 1)
        ((equal? letter 'v) 5)
        ((equal? letter 'x) 10)
        ((equal? letter 'l) 50)
        ((equal? letter 'c) 100)
        ((equal? letter 'd) 500)
        ((equal? letter 'm) 1000)
        (else 'huh?)))