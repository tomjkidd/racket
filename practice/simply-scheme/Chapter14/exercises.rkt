#lang racket
(require (planet dyoo/simply-scheme:2))
(require racket/trace)

#! 14.1
;; keep-like
(define (remove-once wd sent)
  (let ((first-wd (first sent)))
    (if (equal? wd first-wd)
        (bf sent)
        (se first-wd
            (remove-once wd (bf sent))))))

(trace remove-once)
(remove-once 'morning '(good morning good morning))

#! 14.2
;; every-like
(define (up wd)
  (if (empty? wd)
      '()
      (se (up (bl wd))
          wd)))
(trace up)
(up 'town)

#! 14.3
(define (remdup-rec encountered sent)
  (cond ((empty? sent) encountered)
        ((member? (first sent) encountered) (remdup-rec encountered (bf sent)))
        (else (remdup-rec (se encountered (first sent)) (bf sent)))))
           
(define (remdup sent)
  (remdup-rec '() sent))
(trace remdup-rec)
(remdup '(ob la di ob la da))