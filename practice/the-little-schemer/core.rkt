#lang racket

(provide atom? lat? s-expr? count-s-expr member?)

;; An atom is not a pair and not null (the empty list)
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat-book?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define lat?
  (lambda (lst)
    (all-true? (lat-helper lst))))

(define (lat-helper lst)
  (if (null? lst)
      '()
      (cons (atom? (car lst))
            (lat-helper (cdr lst)))))

(define (all-true? bools)
  (foldl (lambda (x result)
           (and x result)) #t bools))

(define s-expr?
  (lambda (x)
    (or (atom? x)
        (list? x))))

(define (count-s-expr lst)
  (count-s-expr-helper lst 0))

(define (count-s-expr-helper lst count)
  (if (null? lst)
      count
      (let ((to-add (if (s-expr? (car lst))
                        1
                        0)))
        (count-s-expr-helper (cdr lst) (+ count to-add)))))

(define member-book?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

(define member?
  (lambda (needle haystack)
    (cond ((null? haystack) #f)
          ((eq? needle (car haystack)) #t)
          (else (member? needle (cdr haystack))))))
