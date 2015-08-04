#lang racket
(require (planet dyoo/simply-scheme:2))
(require racket/trace)

(define (functions)
  (show "Welcome to the FUNCTIONS program.")
  (functions-loop))

(define (functions-loop)
  (let ((fn-name (get-fn)))
    (if (equal? fn-name 'exit)
        "Thanks for using FUNCTIONS!"
        (let ((args (get-args (arg-count fn-name))))
          (if (not (in-domain? args fn-name))
              (show "Argument(s) not in domain.")
              (show-answer (apply (scheme-procedure fn-name) args)))
          (functions-loop)))))

(define (get-fn)
  (display "Function: ")
  (let ((line (read-line)))
    (cond ((empty? line)
	   (show "Please type a function!")
	   (get-fn))
	  ((not (= (count line) 1))
	   (show "You typed more than one thing!  Try again.")
	   (get-fn))
	  ((not (valid-fn-name? (first line)))
	   (show "Sorry, that's not a function.")
	   (get-fn))
	  (else (first line)))))

(define (get-args n)
  (if (= n 0)
      '()
      (let ((first (get-arg)))
        (cons first (get-args (- n 1))))))

(define (get-arg)
  (display "Argument: ")
  (let ((line (read-line)))
    (cond ((empty? line)
	   (show "Please type an argument!")
	   (get-arg))
	  ((and (equal? "(" (first (first line)))
		(equal? ")" (last (last line))))
	   (let ((sent (remove-first-paren (remove-last-paren line))))
	     (if (any-parens? sent)
		 (begin
		  (show "Sentences can't have parentheses inside.")
		  (get-arg))
		 (map booleanize sent))))
	  ((any-parens? line)
	   (show "Bad parentheses")
	   (get-arg))
	  ((empty? (bf line)) (booleanize (first line)))
	  (else (show "You typed more than one argument!  Try again.")
		(get-arg)))))

(define (any-parens? line)
  (let ((letters (accumulate word line)))
    (or (member? "(" letters)
        (member? ")" letters))))

(define (remove-first-paren line)
  (if (equal? (first line) "(")
      (bf line)
      (se (bf (first line)) (bf line))))

(define (remove-last-paren line)
  (if (equal? (last line) ")")
      (bl line)
      (se (bl line) (bl (last line)))))

(define (booleanize x)
  (cond ((equal? x "#t") #t)
        ((equal? x "#f") #f)
        (else x)))

(define (show-answer answer)
  (newline)
  (display "The result is: ")
  (if (not answer)
      (show "#F")
      (show answer))
  (newline))

(define (scheme-procedure fn-name)
  (cadr (assoc fn-name *the-functions*)))

(define (arg-count fn-name)
  (caddr (assoc fn-name *the-functions*)))

;; type-predicate is a function used to make sure that the supplied arguments for
;; a function pass a test for correct types.
(define (type-predicate fn-name)
  (cadddr (assoc fn-name *the-functions*)))

(define (in-domain? args fn-name)
  (apply (type-predicate fn-name) args))

;; Type predicates
(define (word-or-sent? x)
  (or (word? x) (sentence? x)))

(define (not-empty? x)
  (and (word-or-sent? x) (not (empty? x))))

(define (two-numbers? x y)
  (and (number? x) (number? y)))

(define (two-reals? x y)
  (and (real? x) (real? y)))

(define (two-integers? x y)
  (and (integer? x) (integer? y)))

(define (can-divide? x y)
  (and (number? x) (number? y) (not (= y 0))))

(define (dividable-integers? x y)
  (and (two-integers? x y) (not (= y 0))))

(define (trig-range? x)
  (and (number? x) (<= (abs x) 1)))

(define (hof-types-ok? fn-name stuff range-predicate)
  (and (valid-fn-name? fn-name)
       (= 1 (arg-count fn-name))
       (word-or-sent? stuff)
       (empty? (keep (lambda (element)
                       (not ((type-predicate fn-name) element)))
                     stuff))
       (null? (filter (lambda (element)
                        (not (range-predicate element)))
                      (map (scheme-procedure fn-name)
                           (every (lambda (x) x) stuff))))))

(define (member-types-ok? small big)
  (and (word? small)
       (or (sentence? big) (and (word? big) (= (count small) 1)))))

;; Names of functions as functions
(define (named-every fn-name list)
  (every (scheme-procedure fn-name) list))

(define (named-keep fn-name list)
  (every (scheme-procedure fn-name) list))

(define (valid-fn-name? name)
  (assoc name *the-functions*))

;; The list itself
          
(define *the-functions*
  (list (list '* * 2 two-numbers? '(Both arguments must be numbers.))
	(list '+ + 2 two-numbers?)
	(list '- - 2 two-numbers?)
	(list '/ / 2 can-divide?)
	(list '< < 2 two-reals?)
	(list '<= <= 2 two-reals?)
	(list '= = 2 two-numbers?)
	(list '> > 2 two-reals?)
	(list '>= >= 2 two-reals?)
	(list 'abs abs 1 real?)
	(list 'acos acos 1 trig-range?)
	(list 'and (lambda (x y) (and x y)) 2 
	      (lambda (x y) (and (boolean? x) (boolean? y))))
	(list 'appearances appearances 2 member-types-ok?)
	(list 'asin asin 1 trig-range?)
	(list 'atan atan 1 number?)
	(list 'bf bf 1 not-empty?)
	(list 'bl bl 1 not-empty?)
	(list 'butfirst butfirst 1 not-empty?)
	(list 'butlast butlast 1 not-empty?)
	(list 'ceiling ceiling 1 real?)
	(list 'cos cos 1 number?)
	(list 'count count 1 word-or-sent?)
	(list 'equal? equal? 2 (lambda (x y) #t))
	(list 'even? even? 1 integer?)
	(list 'every named-every 2
	      (lambda (fn stuff)
		(hof-types-ok? fn stuff word-or-sent?)))
	(list 'exit '() 0 '())
	   ; in case user applies number-of-arguments to exit
	(list 'exp exp 1 number?)

	(list 'expt expt 2
	      (lambda (x y)
		(and (number? x) (number? y)
		     (or (not (real? x)) (>= x 0) (integer? y)))))
	(list 'first first 1 not-empty?)
	(list 'floor floor 1 real?)
	(list 'gcd gcd 2 two-integers?)
	(list 'if (lambda (pred yes no) (if pred yes no)) 3
	      (lambda (pred yes no) (boolean? pred)))
	(list 'item item 2
	      (lambda (n stuff)
		(and (integer? n) (> n 0)
		     (word-or-sent? stuff) (<= n (count stuff)))))
	(list 'keep named-keep 2
	      (lambda (fn stuff)
		(hof-types-ok? fn stuff boolean?)))
	(list 'last last 1 not-empty?)
	(list 'lcm lcm 2 two-integers?)
	(list 'log log 1 (lambda (x) (and (number? x) (not (= x 0)))))
	(list 'max max 2 two-reals?)
	(list 'member? member? 2 member-types-ok?)
	(list 'min min 2 two-reals?)
	(list 'modulo modulo 2 dividable-integers?)
	(list 'not not 1 boolean?)
	(list 'number-of-arguments arg-count 1 valid-fn-name?)
	(list 'odd? odd? 1 integer?)
	(list 'or (lambda (x y) (or x y)) 2
	      (lambda (x y) (and (boolean? x) (boolean? y))))
	(list 'quotient quotient 2 dividable-integers?)
	(list 'random random 1 (lambda (x) (and (integer? x) (> x 0))))
	(list 'remainder remainder 2 dividable-integers?)
	(list 'round round 1 real?)
	(list 'se se 2
	      (lambda (x y) (and (word-or-sent? x) (word-or-sent? y))))
	(list 'sentence sentence 2
	      (lambda (x y) (and (word-or-sent? x) (word-or-sent? y))))
	(list 'sentence? sentence? 1 (lambda (x) #t))
	(list 'sin sin 1 number?)
	(list 'sqrt sqrt 1 (lambda (x) (and (real? x) (>= x 0))))
	(list 'tan tan 1 number?)
	(list 'truncate truncate 1 real?)
	(list 'vowel? (lambda (x) (member? x '(a e i o u))) 1
	      (lambda (x) #t))
	(list 'word word 2 (lambda (x y) (and (word? x) (word? y))))
	(list 'word? word? 1 (lambda (x) #t))))

;; Retrieve the multiplication function and apply it.
((scheme-procedure '*) 2 3)

#! 21.1
#|
(define (get-args n)
  (if (= n 0)
      '()
      (cons (get-arg) (get-args (- n 1)))))

Compared to...

(define (get-arg)
  (display "Argument: ")
  (let ((line (read-line)))
    (cond ((empty? line)
	   (show "Please type an argument!")
	   (get-arg))
	  ((and (equal? "(" (first (first line)))
		(equal? ")" (last (last line))))
	   (let ((sent (remove-first-paren (remove-last-paren line))))
	     (if (any-parens? sent)
		 (begin
		  (show "Sentences can't have parentheses inside.")
		  (get-arg))
		 (map booleanize sent))))
	  ((any-parens? line)
	   (show "Bad parentheses")
	   (get-arg))
	  ((empty? (bf line)) (booleanize (first line)))
	  (else (show "You typed more than one argument!  Try again.")
		(get-arg)))))

The second way is used so that if there is a mistake, th result of get-arg will not
evaluate to the error, and will only allow a valid arg through.
|#

#! 21.2
#|
(list 'equal? equal? 2 (lambda (x y) #t))

instead of...

(list 'equal? equal? 2 #t)

functions-loop calls in-domain? to do the check, which uses type-predicate to validate
the arguments that are passed in. type-predicated needs a function to do it's work.
#t is not a function, and what we need is a function that always returns true, not
just the #t value.
|#

#! 21.3
;; The main idea here is that the entry should be retrieved and used in functions-loop-21.3
;; This actually simplifies the helper functions.
(define (functions-21.3)
  (show "Welcome to the FUNCTIONS program.")
  (functions-loop-21.3))

(define (functions-loop-21.3)
  (let ((entry (get-fn-21.3)))
    (if (equal? (first entry) 'exit)
        "Thanks for using FUNCTIONS!"
        (let ((args (get-args (arg-count-from-entry entry))))
          (if (not (in-domain-from-entry? args entry))
              (show "Argument(s) not in domain.")
              (show-answer (apply (scheme-procedure-from-entry entry) args)))
          (functions-loop-21.3)))))

(define (get-fn-21.3)
  (display "Function: ")
  (let ((line (read-line)))
    (cond ((empty? line)
	   (show "Please type a function!")
	   (get-fn-21.3))
	  ((not (= (count line) 1))
	   (show "You typed more than one thing!  Try again.")
	   (get-fn-21.3))
	  ((not (valid-fn-name? (first line)))
	   (show "Sorry, that's not a function.")
	   (get-fn-21.3))
	  (else (assoc (first line) *the-functions*)))))

(define (scheme-procedure-from-entry entry)
  (cadr entry))

(define (arg-count-from-entry entry)
  (caddr entry))

;; type-predicate is a function used to make sure that the supplied arguments for
;; a function pass a test for correct types.
(define (type-predicate-from-entry entry)
  (cadddr entry))

(define (in-domain-from-entry? args entry)
  (apply (type-predicate-from-entry entry) args))

;;(functions-21.3)

#! 21.4
;; functions-loop needs to be modified as well as each item in the list of functions.
;; I am going to just provide handling for a single function in order to demonstrate
;; the technique necessary, but don't want to provide a description for everything.
;; All that would be needed is to extend the method if you really want to do it.

(define (error-msg-from-entry? entry)
  (let ((msg (cddddr entry)))
    (if (empty? msg)
        "Argument(s) not in domain."
        msg)))

(define (functions-21.4)
  (show "Welcome to the FUNCTIONS program.")
  (functions-loop-21.4))

(define (functions-loop-21.4)
  (let ((entry (get-fn-21.3)))
    (if (equal? (first entry) 'exit)
        "Thanks for using FUNCTIONS!"
        (let ((args (get-args (arg-count-from-entry entry))))
          (if (not (in-domain-from-entry? args entry))
              (show (error-msg-from-entry? entry))
              (show-answer (apply (scheme-procedure-from-entry entry) args)))
          (functions-loop-21.4)))))

;;(functions-21.4)

#! 21.5
;; 3 arguments are the most that occur in the list.
;; If there is only a single argument: Argument
;; If there is more than 1 upto 3 arguments: '(First Second Third)

(define (functions-21.5)
  (show "Welcome to the FUNCTIONS program.")
  (functions-loop-21.5))

(define (functions-loop-21.5)
  (let ((entry (get-fn-21.3)))
    (if (equal? (first entry) 'exit)
        "Thanks for using FUNCTIONS!"
        (let ((args (get-args-helper (arg-count-from-entry entry))))
          (if (not (in-domain-from-entry? args entry))
              (show (error-msg-from-entry? entry))
              (show-answer (apply (scheme-procedure-from-entry entry) args)))
          (functions-loop-21.4)))))

(define (get-args-helper n)
  (cond ((= n 1) (get-arg-21.5 n '("Argument: ")))
        (else (get-args-21.5 n (list "First Argument: "
                                     "Second Argument: "
                                     "Third Argument: " )))))

(define (get-args-21.5 n display-list)
  (if (= n 0) 
      '()
      (let ((first (get-arg-21.5 (first display-list))))
        (cons first (get-args-21.5 (- n 1) (cdr display-list))))))

(define (get-arg-21.5 to-display)
  (display to-display)
  (let ((line (read-line)))
    (cond ((empty? line)
	   (show "Please type an argument!")
	   (get-arg))
	  ((and (equal? "(" (first (first line)))
		(equal? ")" (last (last line))))
	   (let ((sent (remove-first-paren (remove-last-paren line))))
	     (if (any-parens? sent)
		 (begin
		  (show "Sentences can't have parentheses inside.")
		  (get-arg))
		 (map booleanize sent))))
	  ((any-parens? line)
	   (show "Bad parentheses")
	   (get-arg))
	  ((empty? (bf line)) (booleanize (first line)))
	  (else (show "You typed more than one argument!  Try again.")
		(get-arg)))))

#! 21.6
#|
What happens if a function is requested that isn't in the a-list?
get-fn will call valid-fn-name? before it evaluates arg-count. If #f is returned,
the entry doesn't exist in the list, and arg-count will not be evaluated.
|#

#! 21.7
#|
The word? function knows how to handle input that is not a word.
(lambda (x) #t) means that we allow any input through to word because it's domain
is not limited.
|#

#! 21.8
#|
(functions) returns (functions-loop), which ultimately returns "Thanks for using FUNCTIONS!"
when the user types exit as a function name.
|#

#! 21.9
#|
Based on 21.8, the only reduction that occurs is that the user has an opportunity to
type exit as a function name after evaluating any other function.
|#

;;(functions-21.5)