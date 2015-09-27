#lang racket

(require "core.rkt")
(require "core-7.rkt")
(require racket/trace)

(provide looking
         shift
         align
         length*
         weight*
         shuffle

         C
         A)

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

;; sorn: symbol OR number
(define keep-looking
  (lambda (a sorn lat)
    (cond ((number? sorn)
           (keep-looking a (pick sorn lat) lat))
          (else (eq? sorn a)))))

;; The "most partial" function
(define eternity
  (lambda (x)
    (eternity x)))

(define shift
  (lambda (pair)
    (build (first (first pair))
          (build (second (first pair))
                 (second pair)))))
(trace looking)
(trace keep-looking)

;; pora: pair OR atom.
(define align
  (lambda (pora)
    (cond ((atom? pora) pora)
          ((a-pair? (first pora))
           (align (shift pora)))
          (else (build (first pora)
                       (align (second pora)))))))

(define length*
  (lambda (pora)
    (cond ((atom? pora) 1)
          (else (o+ (length* (first pora))
                    (length* (second pora)))))))

(define weight*
  (lambda (pora)
    (cond ((atom? pora) 1)
          (else (o+ (o* (weight* (first pora)) 2)
                    (weight* (second pora)))))))

(define shuffle
  (lambda (pora)
    (cond ((atom? pora) pora)
          ((a-pair? (first pora))
           (shuffle (revpair pora)))
          (else (build (first pora)
                       (shuffle (second pora)))))))

;; NOTE: Credit to Lothar Collatz
(define C
  (lambda (n)
    (cond ((one? n) 1)
          (else (cond ((even? n) (C (o/ n 2)))
                      (else (C (add1 (o* 3 n)))))))))

;; NOTE: Credit to Wilhelm Ackermann
(define A
  (lambda (n m)
    (cond ((zero? n) (add1 m))
          ((zero? m) (A (sub1 n) 1))
          (else (A (sub1 n)
                   (A n (sub1 m)))))))
