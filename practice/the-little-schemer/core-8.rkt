#lang racket

(require "core.rkt")

(provide rember-f-first
         rember-f
         eq?-c
         rember-eq?
         insertL-f
         insertR-f
         subst-f
         rember-f2
         atom-to-function
         multirember-f
         multirember-eq?
         multiremberT)

(define rember-f-first
  (lambda (fn needle haystack)
    (cond ((null? haystack) '())
          ((fn needle (car haystack))
           (rember-f-first fn needle (cdr haystack)))
          (else
           (cons (car haystack)
                 (rember-f-first fn needle (cdr haystack)))))))

(define rember-f
  (lambda (fn)
    (lambda (needle haystack)
      (cond ((null? haystack) '())
            ((fn needle (car haystack))
             (cdr haystack))
            (else
             (cons (car haystack)
                   ((rember-f fn) needle (cdr haystack))))))))
                             
(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define rember-eq? (rember-f eq?))

(define insertL-f-first
  (lambda (fn)
    (lambda (new old lst)
      (cond ((null? lst) '())
            ((fn (car lst) old)
             (cons new (cons old (cdr lst))))
            (else (cons old ((insertL-f fn) new old (cdr lst))))))))

(define insertR-f-first
  (lambda (fn)
    (lambda (new old lst)
      (cond ((null? lst) '())
            ((fn (car lst) old)
             (cons old (cons new (cdr lst))))
            (else (cons old ((insertR-f fn) new old (cdr lst))))))))

;; Note, the book replaces the insertR and insertL definitions,
;; but I wanted to replace the insertR-f and insertL-f ones.
(define insert-g
  (lambda (seq)
    (lambda (fn)
      (lambda (new old lst)
        (cond ((null? lst) '())
              ((fn (car lst) old)
               (seq new old (cdr lst)))
              (else (cons old (((insert-g seq) fn) new old (cdr lst)))))))))

;; Sequence to used to insert new to the left of old
(define seqL
  (lambda (new old lst)
    (cons new (cons old lst))))

;; Sequence to used to insert new to the right of old
(define seqR
  (lambda (new old lst)
    (cons old (cons new lst))))

;; Sequence to used to insert new and remove old
(define seqS
  (lambda (new old lst)
    (cons new lst)))

;; Sequence to used to remove old
(define seqrem
  (lambda (new old lst)
    lst))

(define insertL-f (insert-g seqL))
(define insertR-f (insert-g seqR))
(define subst-f (insert-g seqS))
(define rember-f2
  (lambda (fn)
    (lambda (needle haystack)
      (((insert-g seqrem) fn) void needle haystack))))

(define multirember-f
  (lambda (fn)
    (lambda (needle haystack)
      (cond ((null? haystack) '())
            ((fn needle (car haystack))
             ((multirember-f fn) needle (cdr haystack)))
            (else (cons (car haystack)
                        ((multirember-f fn) needle (cdr haystack))))))))
(define multirember-eq?
  (multirember-f eq?))

(define multiremberT
  (lambda (fn lat)
    (cond ((null? lat) '())
          ((fn (car lat))
           (multiremberT fn (cdr lat)))
          (else (cons (car lat)
                      (multiremberT fn (cdr lat)))))))
          
          
          