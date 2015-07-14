#lang racket
(require (planet dyoo/simply-scheme:2))
(define (vowel? x) (member? x '(a e i o u y)))

(+ 1 2)
(- 1 2)
(* 1 2 3)
(/ 4 2)
(sqrt 2)
(quotient 3 4)
(remainder 3 5)
(random 9)
(round 1.49)
(max 1 2)
(expt 2 3)

(butfirst 'a)
(count 765432)
(count 'word)
(word 3.14 1592654)
(expt -3 -3)
'(all you need is love)

(sentence '(when i get) 'home)
(butfirst '(yer blues))
(equal? 1 1)
(equal? 'a 'a)
(member? 'the '(wheels on the bus))

(if (equal? 1 1) 'equal 'notequal)

(every first '(the long and winding road))
(keep vowel? 'constantinople)
(even? 2)
(odd? 2)

#! Exercises
#! 2.1
(word 'now 'here) ;; 'nowhere
(sentence 'now 'here) ;; '(now here)
(first 'blackbird) ;; 'b
;;(first ('blackbird)) ;; Error...
(+ 3 4) ;; 7
(every butfirst '(thank you girl)) ;; '(hank ou irl)
(member? 'e 'aardvark) ;; #f
(member? 'the '(take the bus)) ;; #t
;; TODO: Figure out why vowel? was not provided
(keep vowel? '(i will)) ;; '(i)
(keep vowel? 'ebicedifog)
;; (last '()) ;; Error...
(every last '(honey pie)) ;; '(y e)
(keep vowel? 'taxman) ;; 'aa
#! 2.2: Domain of vowel is a character, Range is bools #t and #f

#! 2.3: Domain: int or string, Range: int. Counts how many times the first arg is
#! present in the second arg
(appearances 1 1111) ;; 4

#! 2.4:
#! Domain (when string is second arg): (int > 0, string) indexes into the string, 1 based
(item 4 'abc)