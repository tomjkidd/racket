#lang racket
#|
I want to create an implementation of unify, fresh, #s, #u in order to verify
some of the things that the book is talking about
|#

(define var vector)
(define var? vector?)

(define u (var 'u))
(define v (var 'v))
(define w (var 'w))

(define x (var 'x))
(define y (var 'y))
(define z (var 'z))

(var? x)
(var? y)

(car (cons z 'a))
(cdr (cons z 'a))

(define rhs cdr)
(define lhs car)

(lhs (cons z 'a))
(rhs (cons z w))

;; Here, s stands for substitution
;; A substitution can contain associations
(define empty-s '())

;; Extend a substitution by adding a new pair
(define ext-s
  (lambda (x v s)
    (cons (cons x v) s)))

(define extend-s ext-s)

#|
The book introduced some notation so that:

(cond
  ((assq v s) => f)
  (else v))

is equivalent to

(let ((a (assq v s)))
  (cond
    (a (f a))
    (else v)))
|#

;; Here, v is a var and s is a substitution (association list)
;; Note: assq is like assoc, but finds element using eq?
;; Note: I've also expanded the definition given in TRS.
(define walk
  (lambda (v s)
    (cond
      ((var? v)
       (let ((a (assq v s)))
         (cond
           (a ((lambda (a) (walk (rhs a) s)) a))
           (else v))))
      (else v))))

(define unify
  (lambda (v w s)
    (let ((v (walk v s))
          (w (walk w s)))
      (cond
        ((eq? v w) s)
        ((var? v) (ext-s v w s))
        ((var? w) (ext-s w v s))
        ((and (pair? v) (pair? w))
         (let ((a (unify (car v) (car w) s)))
           (cond
             (a ((lambda (s) (unify (cdr v) (cdr w) s)) a))
             (else #f))))
        (else #f)))))

(define walk*
  (lambda (v s)
    (let ((v (walk v s)))
      (cond
        ((var? v) v)
        ((pair? v)
         (cons
          (walk* (car v) s)
          (walk* (cdr v) s)))
        (else v)))))

(walk z (list (cons z 'a) (cons x w) (cons y z)))

(walk y (list (cons z 'a) (cons x w) (cons y z)))

(walk x (list (cons z 'a) (cons x w) (cons y z)))

(walk w (list (cons x y) (cons w 'b) (cons z x) (cons y z)))

;;(walk x (ext-s x y (list (cons z x) (cons y z)))) ;; Runs forever!

(walk w (ext-s z 3 (list (cons w x) (cons x y) (cons y z))))

(walk x (ext-s z 'b (list (cons y z) (cons x y))))

(walk x (ext-s z w (list (cons y z) (cons x y))))

(unify x y (list (cons x 'a) (cons z 'b)))

(walk* x (list (cons y (list 'a z 'c))
               (cons x y)
               (cons z 'a)))

(walk* x (list (cons y (list z w 'c))
               (cons x y)
               (cons z 'a)))

(walk* y (list (cons y (list w z 'c))
               (cons v 'b)
               (cons x v)
               (cons z x)))