#lang racket

(require "core.rkt")

(provide aexp? ;; arithmetic expression
         numbered?
         value)

(define aexp?
  (lambda (x)
    (cond ((atom? x) #t)
          (else (and (aexp? (1st-sub-exp x))
                     (member? (operator x) '(+ x expt))
                     (aexp? (2nd-sub-exp x)))))))

(define numbered?
  (lambda (aexp)
    (cond ((atom? aexp) (number? aexp))
          ((eq? (operator aexp) '+)
           (and (numbered? (1st-sub-exp aexp))
                (numbered? (2nd-sub-exp aexp))))
          ((eq? (operator aexp) 'expt)
           (and (numbered? (1st-sub-exp aexp))
                (numbered? (2nd-sub-exp aexp))))
          ((eq? (operator aexp) 'x)
           (and (numbered? (1st-sub-exp aexp))
                (numbered? (2nd-sub-exp aexp)))))))

(define value
  (lambda (nexp)
    (cond ((atom? nexp) nexp)
          ((eq? (operator nexp)'+)
           (o+ (value (1st-sub-exp nexp))
               (value (2nd-sub-exp nexp))))
          ((eq? (operator nexp) 'x)
           (o* (value (1st-sub-exp nexp))
               (value (2nd-sub-exp nexp))))
          (else
           (oexpt (value (1st-sub-exp nexp))
                  (value (2nd-sub-exp nexp)))))))

#|
The Eighth Commandment:
Use help functions to abstract from representations
|#
(define 1st-sub-exp
  (lambda (aexp)
    (car aexp)))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car (cdr aexp))))
          
          