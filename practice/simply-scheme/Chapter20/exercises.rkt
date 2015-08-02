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
(define (ask-user-v2 position letter)
  (print-position position)
  (display letter)
  (display "'s move: ")
  (let ((input (read)))
    (if (and (number? input) (>= input 1) (<= input 9))
        input
        (begin (show (se input
                         "is not a valid input. Please choose a position 1-9."))
               (ask-user-v2 position letter)))))

;;(play-ttt ttt ask-user-v2)

#! 20.7
(define (ask-user-v3 position letter)
  (print-position position)
  (display letter)
  (display "'s move: ")
  (let ((input (read)))
    (cond ((and (number? input) (>= input 1) (<= input 9))
           (if (not (equal? (item input position) '_))
               (begin (show (se input
                            "is already occupied. Please choose a free position"))
                       (ask-user-v3 position letter))
               input))
          (else (begin (show (se input
                                 "is not a valid input. Please choose a position 1-9."))
                       (ask-user-v3 position letter))))))

;;(play-ttt ttt ask-user-v3)

#! 20.8
(define (play-ttt-v2 x-strat o-strat)
  (play-ttt-helper-v2 x-strat o-strat '_________ 'x))

(define (play-ttt-helper-v2 x-strat o-strat position whose-turn)
  (cond ((already-won? position (opponent whose-turn))
         (begin
           (print-position position)
           (list (opponent whose-turn) 'wins!)))
        ((tie-game? position) '(tie game))
        (else (let ((square (if (equal? whose-turn 'x)
                                (x-strat position 'x)
                                (o-strat position 'o))))
                (play-ttt-helper-v2 x-strat
                                 o-strat
                                 (add-move square whose-turn position)
                                 (opponent whose-turn))))))

;;(play-ttt-v2 ttt ask-user-v3)

#! 20.9
(define (game)
  (show "Choose x or o")
  (let ((choice (read)))
    (cond ((member? choice '(x o))
           (if (equal? choice 'x)
               (show (play-ttt-v2 ask-user-v3 ttt))
               (show (play-ttt-v2 ttt ask-user-v3))))
          (else (begin
                  (show (se choice
                            "is not x or o"))
                  (game))))))

;;(game)

(define (games)
  (begin
    (game)
    (show "Play again? (y/n)")
    (let ((choice (read)))
      (if (equal? choice 'y)
          (games)
          #f))))

(games)