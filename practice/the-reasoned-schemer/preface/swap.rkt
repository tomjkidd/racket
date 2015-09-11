#lang racket
#|
macros have the following form:
(define-syntax-rule pattern template)

In the following:

swap is an identifier

x and y are macro pattern variables

the body is the template, used in place of a form that matches the pattern
|#
(define-syntax-rule (swap x y)
  (let ((tmp x))
    (set! x y)
    (set! y tmp)))

(define first 1)
(define last 2)

first
last

(swap first last)

first
last

#|
In the case where you need more than one pattern to match against,
define-syntax and syntax-rules can be used.

Use the following form:
(define-syntax id
  (syntax-rules (literal-id ...)
    [pattern template]
    ...))
|#

(define-syntax rotate-first-pass
  (syntax-rules ()
    ((rotate a b) (swap a b))
    ((rotate a b c) (begin
                      (swap a b)
                      (swap b c)))))

(define-syntax rotate-second-pass
  (syntax-rules ()
    ((rotate a) (void))
    ((rotate a b c ...) (begin
                          (swap a b)
                          (rotate b c ...)))))

(define-syntax rotate
  (syntax-rules ()
    ((rotate a c ...)
     (shift-to (c ... a) (a c ...)))))

(define-syntax shift-to
  (syntax-rules ()
    ((shift-to (from0 from ...) (to0 to ...))
     (let ((tmp from0))
       (set! to from) ...
       (set! to0 tmp)))))

(let ((red 1)
      (green 2)
      (blue 3))
  (display red)
  (display green)
  (display blue)
  (newline)
  (rotate red green)
  (display red)
  (display green)
  (display blue)
  (newline)
  (rotate red green blue)
  (display red)
  (display green)
  (display blue)
  (newline))
