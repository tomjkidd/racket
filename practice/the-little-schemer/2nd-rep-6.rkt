#lang racket

(require "core.rkt")

(provide sero?
         edd1
         zub1
         o+2
         lat?2)

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define o+2
  (lambda (n m)
    (cond ((sero? m) n)
          (else (edd1 (o+2 n (zub1 m)))))))

(define lat?2
  (lambda (l)
    (cond ((null? l) #t)
          ((atom? (car l)) (lat?2 (cdr l)))
          (else #f))))