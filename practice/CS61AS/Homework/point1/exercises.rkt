#! Exercise 0 - Introduce Yourself to your Classmates
#|
1) What is your name?
My name is Tom Kidd

2) What is your major?
I went to school at UVM for Electrical Engineering, and then got an academic certificate in Computer Science from BU.

3) Are you a returning student?
I did not take 61AS last semester.

4) What made you take 61AS?
I saw this class on Hacker news, and I have an interest in learning lisp, and this seemed like a current avenue for that.

5) Tell us interesting things about yourself.
I spend a lot of time reading, programming, and listening to podcasts. I like to hear what is going on in the world, and would like to contribute something to it. I value learning and wonder.
|#
#lang racket

#! Exercise 1 - sum-of-squares
(define (square x) (* x x))

(define (sum-of-squares x y) (+ (square x) (square y)))

(square 2)

(sum-of-squares 1 2)

#! Encountered elements of the language
(and (< 1 2) (< 2 3))
(and #t #t) #! true
(and #t #f) #! false
(or #t #f) #! true
(or #f #f) #! false

(define (case-test a b)
  (if(and (number? a)
         (number? b))
     (cond [(= a b) "same"]
           [(< a b) "less"]
           [else "greater"])
     "invalid"))

(case-test 1 1)
(case-test 1 2)
(case-test 2 1)
(case-test "abc" 2)

#! Exercise 2a - can-drive
(define (can-drive age)
  (if(< age 16)
     '(Not yet)
     '(Good to go)))

(can-drive 15)
(can-drive 16)

#! Exercise 2b - fizzbuzz
(define (fizzbuzz num)
  (cond [(and (= (remainder num 3) 0) (=(remainder num 5) 0) 'fizzbuzz)]
        [(= (remainder num 3) 0) 'fizz]
        [(= (remainder num 5) 0) 'buzz]
        [else num]))
(fizzbuzz 3)
(fizzbuzz 5)
(fizzbuzz 15)
(fizzbuzz 2)

#! Exercise 3 - The Most Baffling Question
#! Don't know...

#! Exercise 4 - new-if
(define (infinite-loop) (infinite-loop))
(define (new-if test then-case else-case)
  (if test
      then-case
      else-case))
#|
 This code was used to show that if then-case is (infinite-loop),
 even when test is false the program gets caught in an infinite loop.
|#
