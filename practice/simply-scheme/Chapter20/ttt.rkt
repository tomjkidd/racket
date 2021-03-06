#lang racket
(require (planet dyoo/simply-scheme:2))
(require racket/trace)
#|
Positions are indicated as follows:
|1|2|3|
|4|5|6|
|7|8|9|

Positions are represented in a word, where each index corresponds to a position
123456789

x: x is in the position
o: o is in the position
_: the position is free

|#
(provide (all-defined-out))

(define (find-triples position)
  (every (lambda (comb) (substitute-triple comb position))
         '(123 456 789 147 258 369 159 357)))

(define (substitute-triple combination position)
  (accumulate word
              (every (lambda (square)
                       (substitute-letter square position))
                       combination)))

(define (substitute-letter square position)
  (if (equal? '_ (item square position))
      square
      (item square position)))

(define (my-pair? triple me)
  (and (= (appearances me triple) 2)
       (= (appearances (opponent me) triple) 0)))

(define (opponent letter)
  (if (equal? letter 'x) 'o 'x))

(define (ttt position me)
  (ttt-choose (find-triples position) me))

;; NOTE: Semipredicates are used here in the cond
(define (ttt-choose triples me)
  (cond ((i-can-win? triples me))
        ((opponent-can-win? triples me))
        ((i-can-fork? triples me))
        ((i-can-advance? triples me))
        (else (best-free-square triples))))

(define (i-can-win? triples me)
  (choose-win
   (keep (lambda (triple) (my-pair? triple me)) triples)))

(define (choose-win winning-triples)
  (if (empty? winning-triples)
      #f
      (keep number? (first winning-triples))))

;; NOTE: The inner keep is the same as the keep in i-can-win?
;; It makes sense that we would want to get the value if we encounter it there.
(define (choose-winning-move triples me)
  (keep number? (first (keep (lambda (triple) (my-pair? triple me)) triples))))

(define (opponent-can-win? triples me)
  (i-can-win? triples (opponent me)))

(define (block-opponent-win triples me)
  (first triples))

(define (i-can-fork? triples me)
  (first-if-any (pivots triples me)))

(define (first-if-any sent)
  (if (empty? sent)
      #f
      (first sent)))

(define (pivots triples me)
  (repeated-numbers (keep (lambda (triple) (my-single? triple me)) triples)))

(define (my-single? triple me)
  (and (= (appearances me triple) 1)
       (= (appearances (opponent me) triple) 0)))

(define (repeated-numbers sent)
  (every first
         (keep (lambda (wd) (>= (count wd) 2))
               (sort-digits (accumulate word sent)))))

(define (extract-digit desired-digit wd)
  (keep (lambda (wd-digit) (equal? wd-digit desired-digit)) wd))

(define (sort-digits number-word)
  (every (lambda (digit) (extract-digit digit number-word))
         '(1 2 3 4 5 6 7 8 9)))

(define (opponent-can-fork? triples me)
  (i-can-fork? triples (opponent me)))

(define (i-can-advance? triples me)
  (best-move (keep (lambda (triple) (my-single? triple me)) triples) triples me))

(define (best-move my-triples all-triples me)
  (if (empty? my-triples)
      #f
      (best-square (first my-triples) all-triples me)))

(define (best-square my-triple triples me)
  (best-square-helper (pivots triples (opponent me))
                      (keep number? my-triple)))

(define (best-square-helper opponent-pivots pair)
  (if (member? (first pair) opponent-pivots)
      (first pair)
      (last pair)))

(define (best-free-square triples)
  (first-choice (accumulate word triples)
                '(5 1 3 7 9 2 4 6 8)))

(define (first-choice possibilities preferences)
  (first (keep (lambda (square) (member? square possibilities))
               preferences)))


(substitute-triple 456 '_xo_x_o__)
(substitute-triple 147 '_xo_x_o__)
(substitute-triple 357 '_xo_x_o__)

(find-triples '_xo_x_o__)
(find-triples 'x_____oxo)

(appearances 'o 'oo7)
(appearances 'x 'oo7)
(opponent 'x)
(opponent 'o)
(my-pair? 'oo7 'o)
(my-pair? 'xo7 'o)
(my-pair? 'oox 'o)
(i-can-win? '("1xo" "4x6" o89 "14o" xx8 o69 "1x9" oxo) 'x)
(i-can-win? '("1xo" "4x6" o89 "14o" xx8 o69 "1x9" oxo) 'o)
(choose-winning-move '("1xo" "4x6" o89 "14o" xx8 o69 "1x9" oxo) 'x)
(opponent-can-win? '("1xo" "4x6" o89 "14o" xx8 o69 "1x9" oxo) 'x)
(opponent-can-win? '("1xo" "4x6" o89 "14o" xx8 o69 "1x9" oxo) 'o)
(my-single? "4x6" 'x)
(my-single? 'xo3 'x)
(accumulate word '("4x6" x47 "3x7"))
(extract-digit 7 "4x6x473x7")
(extract-digit 2 "4x6x473x7")
(sort-digits 123456789147258369159357)
(sort-digits "4x6x473x7")
(repeated-numbers '("4x6" x47 "3x7"))
(best-square "78o" (find-triples 'xo__x___o) 'o)
(best-square "36o" (find-triples 'xo__x___o) 'o)
(best-move '("78o" "36o") (find-triples 'xo__x___o) 'o)
(i-can-advance? (find-triples 'xo__x___o) 'o)
(first-choice 123456789147258369159357 '(5 1 3 7 9 2 4 6 8))
(first-choice "1xo4x6o8914oxx8o691x9oxo" '(5 1 3 7 9 2 4 6 8))
(best-free-square (find-triples '_________))
(best-free-square (find-triples '____x____))

#! 10.1
(define (any? lst pred)
  (cond ((null? lst) #f)
        ((pred (first lst)) (first lst))
        (else (any? (bf lst) pred))))

(any? '(1 2 3) (lambda (x) (equal? x 2)))

(define (true-for-all? sent pred)
  (cond ((empty? sent) #f)
        ((empty? (bf sent)) (pred (first sent)))
        (else (and (pred (first sent))
                   (true-for-all? (bf sent) pred)))))
;;(trace true-for-all?)
(true-for-all? 'xxx (lambda (x) (equal? x 'x)))

(define (already-won? position letter)
  (let ((triples (find-triples position)))
    (if (any? triples
              (lambda (triple)
                (true-for-all? triple (lambda (ltr) (equal? ltr letter)))))
        #t
        #f)))

(already-won? 'xxx______ 'x)
(already-won? 'xxx______ 'o)

(ttt 'xxxo_o___ 'o)

#! 10.2
;;(ttt 'oxoxxoxox 'o) ;; Invalid argument to FIRST: (), tie game
(define tie-triples (find-triples 'oxoxxoxox))
(i-can-win? tie-triples 'o)
(opponent-can-win? tie-triples 'o)
(i-can-fork? tie-triples 'o)
(i-can-advance? tie-triples 'o)
;;(best-free-square tie-triples) <- This is what fails.

;; TODO: May want to work in some smarts to check for a win
(define (tie-game? position)
  (true-for-all? position (lambda (x) (member? x '(x o)))))

(tie-game? 'oxoxxoxox)

#! 10.3
(define (tie-game-v2? position letter)
  (let ((triples (find-triples position)))
    (cond ((i-can-win? triples letter) #f)
          (else (tie-game? position)))))

(tie-game-v2? 'oxoxxoxox 'x)
(tie-game-v2? 'oxoxxoxox 'o)
(tie-game-v2? 'o_oxxoxox 'o)

#! Chapter 20

;;;;;;;;;;;;;;;;;;;;;;
;; Actual game
;;;;;;;;;;;;;;;;;;;;;;

(define (stupid-ttt position letter)
  (location '_ position))

(define (location letter word)
  (if (equal? letter (first word))
      1
      (+ 1 (location letter (bf word)))))

(define (play-ttt x-strat o-strat)
  (play-ttt-helper x-strat o-strat '_________ 'x))

(define (play-ttt-helper x-strat o-strat position whose-turn)
  (cond ((already-won? position (opponent whose-turn))
         (list (opponent whose-turn) 'wins!))
        ((tie-game? position) '(tie game))
        (else (let ((square (if (equal? whose-turn 'x)
                                (x-strat position 'x)
                                (o-strat position 'o))))
                (play-ttt-helper x-strat
                                 o-strat
                                 (add-move square whose-turn position)
                                 (opponent whose-turn))))))

(define (add-move square letter position)
  (if (= square 1)
      (word letter (bf position))
      (word (first position)
            (add-move (- square 1) letter (bf position)))))

;;(trace play-ttt-helper)
(play-ttt ttt stupid-ttt)
(play-ttt stupid-ttt ttt)

;;;;;;;;;;;;;;;;;;;;;;
;; Board Display
;;;;;;;;;;;;;;;;;;;;;;
(define (print-position position)
  (print-row (subword position 1 3))
  (show "-+-+-")
  (print-row (subword position 4 6))
  (show "-+-+-")
  (print-row (subword position 7 9))
  (newline))

(define (print-row row)
  (maybe-display (first row))
  (display "|")
  (maybe-display (first (bf row)))
  (display "|")
  (maybe-display (last row))
  (newline))

(define (maybe-display letter)
  (if (not (equal? letter '_))
      (display letter)
      (display " ")))

(define (subword wd start end)
  ((repeated bf (- start 1))
   ((repeated bl (- (count wd) end))
    wd)))

;;;;;;;;;;;;;;;;;;;;;;
;; Accepting User input
;;;;;;;;;;;;;;;;;;;;;;
(define (ask-user position letter)
  (print-position position)
  (display letter)
  (display "'s move: ")
  (read))

;;(define (print-position position)
;;  (show position))

;;(print-position '_x_oo__xx)
;;(play-ttt ttt ask-user)
