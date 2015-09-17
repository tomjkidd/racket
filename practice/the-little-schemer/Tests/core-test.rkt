#lang racket

(require rackunit
         rackunit/text-ui
         "../core.rkt")

(define atom?-tests
  (test-suite
   "Tests for atom?"

   (test-case
    "atom? should return false for '()"
    (check-eq? (atom? '()) #f))

  (test-case
    "atom? should return false for a list"
    (check-eq? (atom? '(a b c)) #f))

  (test-case
    "atom? should return true for a string"
    (check-eq? (atom? 'a) #t))

  (test-case
    "atom? should return true for a number"
    (check-eq? (atom? 42) #t))))

(define member?-tests
  (test-suite
   "Tests for member?"

   (test-case
    "member? should find an element that is present"
    (let ((needle 'a)
          (haystack '(a b c)))
      (check-eq? (member? needle haystack) #t)))

   (test-case
    "member? should not find an element that is missing"
    (let ((needle 'd)
          (haystack '(a b c)))
      (check-eq? (member? needle haystack) #f)))))

;; NOTE: test suites can be composed together so that large collections can be handled consistently
(define all-core-tests
  (test-suite
   "All core tests"
   atom?-tests
   member?-tests))

(run-tests all-core-tests)
    
    