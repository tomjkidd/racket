#lang racket

(require (planet dyoo/simply-scheme:2))
(require racket/trace)

(provide for-each-with-index
         one-based-index-valid?
         delete-if-exists
         file-copy
         save-wrapper
         load-wrapper
         print-file-helper
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

(define (file-op-wrapper filename open-fn proc-fn close-fn)
  (let ((port (open-fn filename)))
    (proc-fn port)
    (close-fn port)))

(define (save-wrapper filename fn)
  (delete-if-exists filename)
  (file-op-wrapper filename open-output-file fn close-output-port))

(define (load-wrapper filename fn)
  (file-op-wrapper filename open-input-file fn close-input-port))

(define (print-file-helper port)
  (let ((stuff (read-string port)))
    (if (eof-object? stuff)
        void
        (begin (show stuff)
               (print-file-helper port)))))

(define (copy-lines inport outport)
  (copy-lines-helper inport outport))

(define (copy-lines-helper inport outport)
  (let ((line (read-string inport)))
    (if (eof-object? line)
        void
        (begin
          (show line outport)
          (copy-lines-helper inport outport)))))

(define (file-copy src-filename target-filename)
  (load-wrapper src-filename
                (lambda (inport)
                  (save-wrapper target-filename
                                (lambda (outport)
                                  (copy-lines inport outport))))))

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