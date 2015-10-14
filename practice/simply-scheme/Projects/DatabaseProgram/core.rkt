#lang racket

(require (planet dyoo/simply-scheme:2))
(require racket/trace)

(provide for-each-with-index
         one-based-index-valid?
         delete-if-exists
         save-wrapper
         sort-mod
         sent-before?)

(define (for-each-with-index fn lst)
  (for-each-with-index-helper lst fn 0))

(define (for-each-with-index-helper lst fn index)
  (if (null? lst)
      void
      (begin
        (fn (car lst) index)
        (for-each-with-index-helper (cdr lst) fn (+ index 1)))))

(define one-based-index-valid?
  (lambda (lst index)
    (and (number? index)
         (<= index (length lst))
         (> index 0))))

(define (delete-if-exists name)
  (if (file-exists? name)
      (delete-file name)
      void))

(define (save-wrapper filename fn)
  (delete-if-exists filename)
  (let ((port (open-output-file filename)))
    (fn port)
    (close-output-port port)))

(define rember
  (lambda (needle haystack)
    (cond ((null? haystack) '())
          ((equal? needle (car haystack)) (cdr haystack))
          (else (cons (car haystack) (rember needle (cdr haystack)))))))

#| Sort reducer allows a function to be used to determine which element
   should come first in the sorted list |#
(define sort-reducer
  (lambda (fn)
    (lambda (r1 r2)
      (cond ((fn r1 r2) r1)
            (else r2)))))

(define sort-mod
  (lambda (fn lst)
    (cond ((null? lst) '())
          (else
           (let ((reduction (reduce (sort-reducer fn) lst)))
             (cons reduction (sort-mod fn (rember reduction lst))))))))

(define (sent-before? sent1 sent2)
  (cond ((null? sent1) #t)
        ((null? sent2) #f)
        ((before? (car sent1) (car sent2)) #t)
        ((before? (car sent2) (car sent1)) #f)
        (else (sent-before? (cdr sent1) (cdr sent2)))))