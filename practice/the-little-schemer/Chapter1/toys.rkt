#lang racket

(require "../core.rkt" racket/trace)

(atom? '())
(atom? 'atom)
(atom? 'turkey)
(atom? 1492)
(atom? 'u)
(atom? '*abc$)
(list? '('atom))
(list? '('atom 'turkey 'or))

;; (list? '(atom 'turkey) 'or) ;; arity mismatch
(list? (list (list 'atom 'turkey) 'or))
(s-expr? 'xyz) ;; all atoms are S-expressions
(s-expr? '(x y z)) ;; lists are S-expressions
(s-expr? (list (list 'x 'y) 'z))
(list? '(how are you doing so far))
(count-s-expr '(how are you doing so far))
(count-s-expr (list (list (list 'how) 'are)
                    (list (list 'you) (list 'doing 'so))
                    'far))

(list? '())
(atom? '())
(list? (list '() '() '() '()))
(car '(a b c))
(car (list '(a b c) 'x 'y 'z))
;;(car 'hotdog) ;; contract violation (cannot ask for the car of an atom)
;;(car '()) ;; contract violation (cannot ask for the car of an empty list)
#|
The Law of Car: The primitive car is defined only for non-empty lists
|#

(car (list (list (list 'hotdogs)) '(and) '(pickle) '(relish)))
(car (car (list (list (list 'hotdogs)) '(and))))
(cdr '(a b c))
(cdr (list '(a b c) 'x 'y 'z))
(cdr '(hamburger))
(cdr (list '(x) 't 'r))
;;(cdr 'hotdogs) ;; contract violation (cannot ask for cdr of an atom

;;(cdr '()) ;; contract violation (cannot ask for the cdr of the null l;;ist)
#|
The Law of Cdr: The primitive cdr is defined only for non-empty lists. The cdr
of any non-empty list is always another list.
|#
(car (cdr (list '(b) '(x y) (list (list 'c)))))
(cdr (cdr (list '(b) '(x y) (list (list 'c)))))
;;(cdr (car (list 'a (list 'b '(c)) 'd))) ;; contract violation (law of cdr)
#| car takes any non-empty list |#
#| cdr takes any non-empty list |#
(cons 'peanut '(butter and jelly)) ;; cons adds an atom to the front of a list


(cons '(banana and) '(peanut butter and jelly))
(cons (list '(help) 'this)
      (list 'is 'very (list '(hard) 'to 'learn)))
#|
cons takes 2 arguments
first one is any S-expression
second one is any list
|#
(cons (list 'a 'b '(c))
      '())
(cons 'a '())
(define example-pair (cons (list '(a b c)) 'b))
example-pair;; This evaluates to a pair, but is called no answer in the book
(car example-pair)
(cdr example-pair)

(define example-pair-2 (cons 'a 'b))
(car example-pair-2)
(cdr example-pair-2)
#| Because 'b is not a list, The Little Schemer seeks to avoid dealing with it |#

#|
The Law of Cons: The primitive cons takes two arguments
The second argument to cons must be a list. The result is a list.
|#
(car (list '(b) 'c 'd)) ;; '(b)
(cons 'a '(b)) ;; '(a b)
(cons 'a (car (list '(b) 'c 'd)))
(cons 'a (cdr (list '(b) 'c 'd)))
(null? '())
(null? (quote ()))
(null? '(a b c))

(null? 'a) ;; Works fine, but should be interpreted as 'No answer'
#|
The Law of Null?
The primitive null? is defined only for lists
|#
(atom? 'Harry)
(atom? '(Harry had a heap of apples))
#| atom? takes one argument. The argument can be any S-expression |#

(atom? (car '(Harry had a heap of apples)))
(atom? (cdr '(Harry had a heap of apples)))
(atom? (cdr '(Harry)))
(atom? (car (cdr '(swing low sweet cherry oat))))
(atom? (car (cdr (list 'swing '(low sweet) 'cherry 'oat))))
(eq? 'Harry 'Harry)
(eq? 'margarine 'butter)

(eq? '() '(strawberry)) ;; No answer, but #f is implemented behavior
(eq? 6 7) ;; No answer, but #f is implemented behavior
#|
The Law of Eq?: The primitive eq? takes two arguments.
Each must be a non-numeric atom.
|#
(eq? (car '(Mary had a little lamb chop)) 'Mary)
(eq? (cdr '(soured milk)) 'milk)

(define l '(beans beans we need jelly beans))
(eq? (car l) (car (cdr l)))
