#lang racket
(require (planet dyoo/simply-scheme:2))
(require racket/trace)

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

#! 12.7
(define (spell-digit digit)
  (item (+ 1 digit)
        '(zero one two three four five six seven eight nine)))

(define (spell-number num)
  (if (empty? num)
      '()
      (se (spell-digit (first num))
          (spell-number (bf num)))))

(spell-number 1971)

#! 12.8
(define (numbers sent)
  (if (empty? sent)
      '()
      (let ((current (first sent)))
        (se (if (number? current)
                current
                 '())
                (numbers (bf sent))))))

(numbers '(76 trombones and 110 cornets))

#! 12.9
(define (real-word? wd)
  (not (member? wd '(a the an in of and for to with))))

(define (real-words sent)
  (if (empty? sent)
      '()
      (let ((current (first sent)))
        (se (if (real-word? current)
                current
                '())
            (real-words (bf sent))))))

(real-words '(it was the best of times it was the worst of times))

#! 12.10
(define (remove2 wd sent)
  (if (empty? sent)
      '()
      (let ((current (first sent)))
        (se (if (equal? wd current)
                '()
                current)
            (remove2 wd (bf sent))))))

(remove2 'the '(the song love of the loved by the beatles))

#! 12.11
(define (count2 input)
  (if (empty? input)
      0
      (+ 1 (count2 (bf input)))))

(count2 'abc)
(count2 '(abc def ghi jkl))

#! 12.12
(define (roman-value letter)
  (cond ((member? letter '(i I)) 1)
        ((member? letter '(v V)) 5)
        ((member? letter '(x X)) 10)
        ((member? letter '(l L)) 50)
        ((member? letter '(c C)) 100)
        ((member? letter '(d D)) 500)
        ((member? letter '(m M)) 1000)
        (else 'huh?)))

(define (has-next? wd)
  (if (> (count wd) 1)
      #t
      #f))

(define (adhere-to-subtract-rule current next)
  (if (< current next)
      (* -1 current)
      current))

(define (arabic roman-letters)
  (if (empty? roman-letters)
    0
    (let ((current (roman-value (first roman-letters))))
      (if (has-next? roman-letters)
          (let ((next (roman-value (first (bf roman-letters)))))
            (+ (adhere-to-subtract-rule current next)
               (arabic (bf roman-letters))))
          (+ current (arabic (bf roman-letters)))))))

(arabic 'MCMLXXI)
(arabic 'MLXVI)

#! 12.13

#|
60 sec/min
60 min/hour
24 hour/day
7  day/week
52 week/year
10 year/decade
10 decade/century

Example:
10decade/century* 10year/decade * 52 week/year * 7day/week * 24hour/day * 60min/hour * 60sec/min -> sec/century

|#

(define weeks-per-year 52)

;; TODO: Could make this more concise, but with less clarity...
(define time-factor-list-with-decades
  (every round (list (* 10 10 weeks-per-year 7 24 60 60) ;; sec/century
                     (* 10 weeks-per-year 7 24 60 60) ;; sec/decade
                     (* weeks-per-year 7 24 60 60) ;; sec/year
                     (* 7 24 60 60) ;; sec/week
                     (* 24 60 60) ;; sec/day
                     (* 60 60) ;; sec/hour
                     60 ;; sec/min
                     1))) ;; sec/sec...

(define time-name-list-with-decades '(centuries decades years weeks days hours minutes seconds))

(define time-factor-list
  (every round (list (* 100 weeks-per-year 7 24 60 60) ;; sec/century
                     (* weeks-per-year 7 24 60 60) ;; sec/year
                     (* 7 24 60 60) ;; sec/week
                     (* 24 60 60) ;; sec/day
                     (* 60 60) ;; sec/hour
                     60 ;; sec/min
                     1))) ;; sec/sec...

(define time-name-list '(centuries years weeks days hours minutes seconds))


(define (get-time-factor index)
  (item (+ 1 index) time-factor-list))

(define (get-time-name index)
  (item (+ 1 index) time-name-list))

(define (describe-time-rec-first index value)
  (if (< index (count time-factor-list))
      (se (quotient value (get-time-factor index))
          (get-time-name index)
          (describe-time-rec-first (+ index 1) (remainder value (get-time-factor index))))
      '()))

(define (describe-time-rec index value)
  (if (< index (count time-factor-list))
      (let ((current (quotient value (get-time-factor index))))
        (se (if (equal? 0 current)
                '()
                (se current (get-time-name index)))
            (describe-time-rec (+ index 1) (remainder value (get-time-factor index)))))
      '()))
  
(define (describe-time secs)
  (describe-time-rec 0 secs))

(trace describe-time-rec)

(describe-time 22222)
;; TODO: My result is off from the one presented, probably due to the weeks/year conversion
(describe-time 4967189641)
(count time-factor-list)