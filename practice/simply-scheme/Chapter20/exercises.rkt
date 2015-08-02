#lang racket
(require (planet dyoo/simply-scheme:2))
(require racket/trace)
(require "ttt.rkt")

(define (bottles n)
  (if (= n 0)
      'burp
      (begin (verse n)
             (bottles (- n 1)))))

(define (verse n)
  (show (cons n '(bottles of beer on the wall)))
  (show (cons n '(bottles of beer)))
  (show '(if one of those bottles should happen to fall))
  (show (cons (- n 1) '(bottles of beer on the wall)))
  (show '()))

(bottles 3)

#! 20.1
(cond ((= 2 3) (show '(lady madonna)) '(i call your name))
      ((< 2 3) (show '(the night before)) '(hello little girl)) ;; <- this is the cond
      (else '(p.s. i love you)))

;; (show '(the night before)) displays (the night before) in purple in DrRacket.
;; '(hello little girl) is the last expression, which is returned (and printed in blue in DrRacket).

#! 20.2
(newline)

#! 20.3
;; Show takes a list as an argument and displays a sentence, each print a separate line
;; Display takes an argument and displays it, without a newline.
(show '(a b c))
(display '(a b c))
(display '(d e f))

(define (show-mod lst)
  (display lst)
  (newline))

(show 'single)
(show-mod 'single)

(show '(two words))
(show-mod '(two words))

(show (+ 3 4))
(show-mod (+ 3 4))

#! 20.4
(define (converse)
  (display '(Hello, I'm the computer. What's your name?))
  (display (se 'Hi (read-line) '( How are you?)))
  (let ((input (read-line)))
    (display (cond ((equal? input '(fine)) '(glad to hear it))
                   (else '(hmmm. I don't know what to say))))))

;;(converse)

#! 20.5
(define (max-last-name-length names)
  (reduce max (map (lambda (name) (count (car (cdr name)))) names)))

(define (name-table-helper names col-width)
  (if (null? names)
      'done
      (begin (display (align (cadar names) col-width))
	     (show (caar names))
	     (name-table-helper (cdr names) col-width))))

(define (name-table names)
  (name-table-helper names (+ (max-last-name-length names) 2)))

(max-last-name-length '((john lennon) (paul mccartney) (george harrison) (ringo starr)))

(name-table '((john lennon) (paul mccartney) (george harrison) (ringo starr)))
(name-table '((piotr tchaikovsky) (nicolay rimsky-korsakov)
		(sergei rachmaninov) (modest musorgsky)))

(name-table '((bill evans) (paul motian) (scott lefaro)))

#! 20.6