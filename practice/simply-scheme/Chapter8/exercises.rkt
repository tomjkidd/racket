#lang racket
(require (planet dyoo/simply-scheme:2))

(define (first-letters sent)
  (every first sent))

(first-letters '(here comes the sun))

(define (plural noun)
  (if (equal? (last noun) 'y)
      (word (bl noun) 'ies)
      (word noun 's)))

(every plural '(beatle turtle holly kink zombie))

(define (double letter) (word letter letter))

(every double 'girl)

(keep even? '(1 2 3 4 5))

(define (ends-e? word) (equal? (last word) 'e))
(keep ends-e? '(please put the salami above the blue elephant))

(keep number? '(1 after 909))

(keep number? 'zonk23hey9)

(define (vowel? letter) (member? letter '(a e i o u)))

(keep vowel? 'piggies)

(define (real-word? wd)
  (not (member? wd '(a the an in of and for to with))))

(keep real-word? '(lucy in the sky with diamonds))

(every first (keep real-word? '(lucy in the sky with diamonds)))

(accumulate + '(6 3 4 -5 7 8 9))
(accumulate word '(a c l u))
(accumulate max '(128 32 134 136))

(define (hyphenate word1 word2)
  (word word1 '- word2))

(accumulate hyphenate '(ob la di ob la da))

((repeated bf 3) '(she came in through the bathroom window))

#! 8.1
(define square (lambda (x) (* x x)))
(every last '(algebra purple spaghetti tomato gnu)) ;; '(a e i o u)
(keep number? '(one two three four)) ;; '()
(accumulate * '(6 7 13 0 9 42 17)) ;; 0
(member? 'h (keep vowel? '(t h r o a t))) ;; #f
(every square (keep even? '(87 4 7 12 0 5))) ;; '(16 144 0)
(accumulate word (keep vowel? (every first '(and i lover her)))) ;; 'ai
((repeated square 0) 25) ;; 25
(every (repeated bl 2) '(good day sunshine)) ;; '(go d sunshi)

#! 8.2
(keep vowel? 'birthday) ;; 'ia
(every first '(golden slumbers)) ;; '(g s)
(first '(golden slumbers)) ;; 'golden
(every last '(little child)) ;; '(e d)
(accumulate word (every last '(little child)))
(every + '(2 3 4 5)) ;; '(2 3 4 5)
(accumulate + '(2 3 4 5)) ;; 14

#! 8.3
(define (f a)
  (keep even? a))
#| f is a function of one argument, a.
   The argument is a list of numbers.
   The result is the list filtered down to just even numbers
|#
(f '(0 1 2 3 4 5 6)) ;; '(0 2 4 6)

(define (g b)
  (every b '(blue jay way)))
#| g is a function of one argument, b
   The argument is a function to apply to a list of words
   The result is a list of results for the function called on each word in '(blue jay way)
|#
(g first) ;; '(b j w)
(g last) ;; '(e y y)
(g word) ;; '(blue jay way)
(g (repeated bl 1)) ;; '(blu ja wa)

(define (h c d)
  (c (c d)))
#| h is a function of two arguments, c and d
   c: function that takes one parameter
   d: argument to provide the function
   result: The function applied twice to the argument
|#
(h square 2) ;; 16
(h bl 'abcd) ;; 'ab
(h bf '(this is the way));; '(the way)

(define (i e)
  (/ (accumulate + e) (count e)))
#| i is  function of one argument, e.
   e: a list of numbers
   result: takes the sum and divides it by the count.
|#

(i '(1 2)) ;; 3/2
(i '(1 2 3)) ;; 2

accumulate
#| accumulate is a function of two arguments, a function and a list
   The function is applied to the set of all arguments
   The result is a single value
|#
(accumulate + '(1 2 3)) ;; 6

sqrt
#| sqrt is a function of one argument.
   The argument is a number.
   The result is the square root of the number.
|#
(sqrt 16) ;; 4
(sqrt 2) ;; 1.4142...

repeated
#| repeated is a function that takes two arguments, a b
   a: A function to call
   b: The number of times to repeatedly call the function a
   The result is a function that will perform the composition of the function a called b times with a single argument.
|#
(every (repeated bl 2) '(phone call)) ;; '(pho ca)

(define triple-sqrt (repeated sqrt 3))
(triple-sqrt 256)

(define even-contract-violator (repeated even? 2))
;; (even-twice 2) ;; Error: even? returns a boolean, and then you try to apply even? to a bool, which is a contract violation.

(define first-twice (repeated first 2))
(first-twice (list '(abc def ghi) '(abc def ghi))) ;; Didn't technically introduce list, but I wanted to try this one out.
(first-twice '(abc def ghi)) ;; 'a, when called on a sentence, first letter of first word
(first-twice 'abc) ;; 'a, when called on a word, redundant call to first letter

(define skip-six (repeated (repeated bf 3) 2))
(skip-six 'abcdefghijkl) ;; 'ghijkl, skips the first six letters
(skip-six '(abc def ghi jkl mno pqr stu vwx yz)) ;; '(stu vwx yz), skips the first 6 words

#! 8.4 -> This is a filter opertion.
(define beatles '(john paul george ringo))

(define (choose-beatles pred)
  (keep pred beatles))

(define (ends-vowel? wd) (vowel? (last wd)))
(define (even-count? wd) (even? (count wd)))

(choose-beatles ends-vowel?)
(choose-beatles even-count?)

#! 8.5 -> This is a map operation
(define (transform-beatles transform)
  (every transform beatles))

(define (amazify name)
  (word 'the-amazing- name))

(transform-beatles amazify)
(transform-beatles butfirst)

#! 8.6
(define (to-nato-phonetic letter) 
  (let ((letter-equal-to? (lambda (x) (equal? letter x))))
    (cond ((letter-equal-to? 'a) 'alpha)
          ((letter-equal-to? 'b) 'bravo)
          ((letter-equal-to? 'c) 'charlie)
          ((letter-equal-to? 'd) 'delta)
          ((letter-equal-to? 'e) 'echo)
          ((letter-equal-to? 'f) 'foxtrot)
          ((letter-equal-to? 'g) 'golf)
          ((letter-equal-to? 'h) 'hotel)
          ((letter-equal-to? 'i) 'india)
          ((letter-equal-to? 'j) 'juliet)
          ((letter-equal-to? 'k) 'kilo)
          ((letter-equal-to? 'l) 'lima)
          ((letter-equal-to? 'm) 'mike)
          ((letter-equal-to? 'n) 'november)
          ((letter-equal-to? 'o) 'oscar)
          ((letter-equal-to? 'p) 'papa)
          ((letter-equal-to? 'q) 'quebec)
          ((letter-equal-to? 'r) 'romeo)
          ((letter-equal-to? 's) 'sierra)
          ((letter-equal-to? 't) 'tango)
          ((letter-equal-to? 'u) 'uniform)
          ((letter-equal-to? 'v) 'victor)
          ((letter-equal-to? 'w) 'whiskey)
          ((letter-equal-to? 'x) 'xray)
          ((letter-equal-to? 'y) 'yankee)
          ((letter-equal-to? 'z) 'zulu))))

(define (words wd)
  (every (lambda (x) (every to-nato-phonetic x)) wd))

(to-nato-phonetic 'a)
(to-nato-phonetic 'b)
(words '(tom))
(words '(kidd))

#! 8.7
(define (letter-count-word word)
  (every (lambda (x) 1) word))

(define (letter-count sent)
  (accumulate + (every letter-count-word sent)))

(letter-count '(abc))
(letter-count '(abc def))
(letter-count '(fixing a hole))

#! 8.8
(define (exaggerate-word wd)
  (cond ((number? wd) (* 2 wd))
        ((equal? 'good wd) 'great)
        ((equal? 'bad wd) 'terrible)
        (else wd)))
(define (exaggerate sent)
  (every exaggerate-word sent))

(exaggerate-word 3)
(exaggerate-word 'good)
(exaggerate '(i ate 3 potstickers))
(exaggerate '(the chow fun is good here))

#! 8.9
(define example-sentence '(all words should be unchanged))

(define (every-identity sent)
  (every word sent))
(every-identity example-sentence)

(define (keep-identity sent)
  (keep word? sent))
(keep-identity example-sentence)

(define (accumulate-identity sent)
  (accumulate sentence sent))
(accumulate-identity example-sentence)

#! 8.10
(define (true-for-all? pred sent)
  ;; Solution from https://github.com/buntine/Simply-Scheme-Exercises
  (= (count sent) (count (keep pred sent))))
;; TODO: Find out why this doesn't work...
;; Look at how every is implemented
;; http://planet.racket-lang.org/package-source/dyoo/simply-scheme.plt/2/2/simply-scheme.rkt
;;(and (every first sent)))
(true-for-all? even? '(2 4 6 8))
(true-for-all? even? '(2 6 3 4))

#! 8.11
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
(base-grade 'A)
(base-grade 'F)
(grade-modifier 'A+)
(grade-modifier 'A)
(grade-modifier 'A-)

(define (single-gpa grade)
  (+ (base-grade grade) (grade-modifier grade)))

(single-gpa 'A+)
(single-gpa 'A)
(single-gpa 'A-)

(define (gpa grades)
  (/ (accumulate + (every single-gpa grades)) (count grades)))

;; TODO: Create an arbitrary round procedure...
(gpa '(A A+ B+ B))

#! 8.12
(define (equals-um? wd)
  (equal? wd 'um))
(define (always-1 x)
  1)
(define (count-ums sent)
  (accumulate + (every always-1 (keep equals-um? sent))))

(define (count-ums2 sent)
  (count (keep equals-um? sent)))

(count-ums '(today um we are going to um talk about functional um programming))

#! 8.13
;; Using https://en.wikipedia.org/wiki/Telephone_keypad
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
  (accumulate word (every unspell-letter wd)))

(phone-unspell 'popcorn)

#! 8.14
;; TODO: Research into ways other people have solved this, feels like the way I did it is pretty involved
(define (next x)
  (+ x 1))
(define (build from to current)
  (if (> from to)
      current
      (build (next from) to (se current from))))

(build 5 8 '())

(define (subword wd start-pos end-pos)
  (define (in-between x)
    (and (>= x start-pos) (<= x end-pos)))
  (define (letter-at x)
    (item x wd))
  (accumulate word (every letter-at (build start-pos end-pos '()))))

(subword 'polythene 5 8)

