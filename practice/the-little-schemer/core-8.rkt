#lang racket

(require "core.rkt")
(require racket/trace)

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
         multiremberT

         multirember&co
         multiinsertLR
         multiinsertLR&co

         evens-only*
         evens-only*&co)

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

#|
col is a function that takes 2 arguments
It is short for "collector", and sometimes called a "continuation"
|#
(define multirember&co
  (lambda (a lat col)
    (cond ((null? lat)
           (col '() '()))
          ((eq? (car lat) a)
           (multirember&co a
                           (cdr lat)
                           (lambda (newlat seen)
                             (col newlat
                                  (cons (car lat) seen)))))
          (else
           (multirember&co a
                           (cdr lat)
                           (lambda (newlat seen)
                             (col (cons (car lat) newlat)
                                  seen)))))))

(define multiinsertLR
  (lambda (new oldL oldR lst)
    (cond ((null? lst) '())
          ((eq? (car lst) oldL)
           (cons new
                 (cons oldL
                       (multiinsertLR new oldL oldR (cdr lst)))))
          ((eq? (car lst) oldR)
           (cons oldR
                 (cons new
                       (multiinsertLR new oldL oldR (cdr lst)))))
          (else (cons (car lst)
                      (multiinsertLR new oldL oldR (cdr lst)))))))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond ((null? lat) (col '() 0 0))
          ((eq? (car lat) oldL)
           (multiinsertLR&co new oldL oldR
                             (cdr lat)
                             (lambda (newlat L R)
                               (col (cons new (cons oldL newlat))
                                    (add1 L)
                                    R))))
          ((eq? (car lat) oldR)
           (multiinsertLR&co new oldL oldR
                             (cdr lat)
                             (lambda (newlat L R)
                               (col (cons oldR (cons new newlat))
                                    L
                                    (add1 R)))))
          (else (multiinsertLR&co new oldL oldR
                             (cdr lat)
                             (lambda (newlat L R)
                               (col (cons (car lat) newlat)
                                    L
                                    R)))))))

(define evens-only*
  (lambda (lst)
    (cond ((null? lst) '())
          ((atom? (car lst))
           (cond ((even? (car lst))
                  (cons (car lst)
                        (evens-only* (cdr lst))))
                 (else (evens-only* (cdr lst)))))
          (else (cons (evens-only* (car lst))
                      (evens-only* (cdr lst)))))))

(define even?
  (lambda (n)
    (o= (o* (o/ n 2) 2) n)))

(define evens-only*&co
  (lambda (lst col)
    (cond ((null? lst) (col lst 1 0))
          ((atom? (car lst))
           (cond ((even? (car lst))
                  (evens-only*&co (cdr lst)
                                 (lambda (newlst p s)
                                   (col (cons (car lst) newlst)
                                        (o* (car lst) p)
                                        s))))
                 (else (evens-only*&co (cdr lst)
                                       (lambda (newlst p s)
                                         (col newlst
                                              p
                                              (o+ (car lst) s)))))))
          (else (evens-only*&co (car lst)
                                (lambda (al ap as)
                                  (evens-only*&co (cdr lst)
                                                  (lambda (dl dp ds)
                                                    (col (cons al dl)
                                                         (o* ap dp)
                                                         (o+ as ds))))))))))
          
          