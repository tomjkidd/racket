#lang racket

(require "core.rkt"
         racket/trace)

(provide set?
         makeset
         subset?
         eqset?
         intersect?
         intersect
         union
         difference
         intersectall
         a-pair?

         first
         second
         build

         rel?
         fun?

         revrel

         fullfun?
         one-to-one?

         seconds)

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

(define difference
  (lambda (set1 set2)
    (cond ((null? set1) '())
          ((member? (car set1) set2)
           (difference (cdr set1) set2))
          (else (cons (car set1)
                      (difference (cdr set1) set2))))))

(define intersectall
  (lambda (list-of-sets)
    (cond ((null? (cdr list-of-sets)) (car list-of-sets))
          (else (intersect (car list-of-sets)
                           (intersectall (cdr list-of-sets)))))))

(define a-pair?
  (lambda (x)
    (cond ((atom? x) #f)
          ((null? x) #f)
          ((null? (cdr x)) #f)
          ((null? (cdr (cdr x))) #t)
          (else #f))))

(define first
  (lambda (lst)
    (car lst)))

(define second
  (lambda (lst)
    (car (cdr lst))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define third
  (lambda (lst)
    (car (cdr (cdr lst)))))

(define rel?
  (lambda (rel)
    ;; a relation is a set of pairs
    (and (= 0 (length (filter (lambda (p) (not (pair? p))) rel)))
         (set? rel))))

(define fun?
  (lambda (rel)
    ;; a function has mappings x -> y, each x occurs once
    (set? (firsts rel))))

(define revpair
  (lambda (p)
     (build (second p)
            (first p))))

(define revrel
  (lambda (rel)
    (cond ((null? rel) '())
          (else (cons (revpair (car rel))
                      (revrel (cdr rel)))))))

(define fullfun?
  (lambda (fun)
    (fun? (revrel fun))))

(define one-to-one? fullfun?)

(define seconds
  (lambda (lst)
    (map (lambda (p) (car (cdr p))) lst)))
    
           