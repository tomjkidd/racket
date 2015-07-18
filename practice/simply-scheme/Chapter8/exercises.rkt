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
(first-twice '(abc def ghi)) ;; 'a, when called on a sentence, first word of first sentence
(first-twice 'abc) ;; 'a, when called on a word, redundant call to first

(define skip-six (repeated (repeated bf 3) 2))
(skip-six 'abcdefghijkl) ;; 'ghijkl, skips the first six letters
(skip-six '(abc def ghi jkl mno pqr stu vwx yz)) ;; '(stu vwx yz), skips the first 6 words