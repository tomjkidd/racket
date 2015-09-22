#lang racket

(require "core.rkt")

(provide set?
         makeset
         subset?
         eqset?
         intersect?
         intersect
         union)

(define set?
  (lambda (lst)
    (cond ((null? lst) #t)
          ((member? (car lst) (cdr lst)) #f)
          (else (set? (cdr lst))))))

(define makeset-first
  (lambda (lst)
    (cond ((null? lst) '())
          ((member? (car lst) (cdr lst)) (makeset (cdr lst)))
          (else (cons (car lst) (makeset (cdr lst)))))))

(define makeset
  (lambda (lst)
    (cond ((null? lst) '())
          (else (cons (car lst)
                      (makeset (multirember (car lst) (cdr lst))))))))

(define subset?
  (lambda (subset set)
    (cond ((null? subset) #t)
          (else (cond ((member? (car subset) set)
                       (subset? (cdr subset) set))
                      (else #f))))))

(define eqset?-first
  (lambda (set1 set2)
    (cond ((subset? set1 set2)
           (subset? set2 set1))
          (else #f))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

;; At least one item in set1 is in set 2
(define intersect?
  (lambda (set1 set2)
    (cond ((null? set1) #f)
          ((member? (car set1) set2) #t)
          (else (intersect? (cdr set1) set2)))))

(define intersect
  (lambda (set1 set2)
    (cond ((null? set1) '())
          ((member? (car set1) set2)
           (cons (car set1)
                 (intersect (cdr set1) set2)))
          (else (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond ((null? set1) set2)
          ((member? (car set1) set2)
           (union (cdr set1) set2))
          (else (cons (car set1) (union (cdr set1) set2))))))
           