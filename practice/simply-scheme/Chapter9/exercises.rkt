#lang racket
(require (planet dyoo/simply-scheme:2))

(define (backwards wd)
  (accumulate (lambda (x y) (word y x)) wd))

(backwards 'yesterday)
(every backwards '(i saw her standing there))

(define (keeper letter)
  (lambda (sent)
    (keep (lambda (wd) (member? letter wd)) sent)))
(define keep-h (keeper 'h))

(keep-h '(i have a secret hoard of hamburgers))

#! 9.1
(lambda (x) (+ (* x 3) 4)) ;; Just a procedure
((lambda (x) (+ (* x 3) 4)) 10) ;; Invoking the procedure with 10 as the argument -> 34
(every (lambda (wd) (word (last wd) (bl wd))) '(any time at all)) ;; '(yan etime ta lal)
;; ((lambda (x) (+ x 3)) 10 15) ;; Error, too many arguments

#! 9.2
(define (second stuff)
  (first (bf stuff)))
(define (second-lambda stuff)
  ((lambda (x) (first (bf x))) stuff))

(second-lambda 'abc)
(second-lambda '(a b c))

(define (make-adder num)
  (lambda (x) (+ num x)))
(define (make-adder-lambda num)
  ((lambda (y) (lambda (x) (+ y x))) num))

((make-adder-lambda 4) 3)

#! 9.3
(define (let-it-be sent)
  (accumulate (lambda (x y) y) sent)) ;; Only keeps the last letter of a word, last word of a sentence
(let-it-be 'abcde)
(let-it-be '(abc def ghi jkl mno))

#! 9.4
(define (who sent)
  (every (lambda (person) (se person sent)) '(pete roger john keith)))

(who '(sells out))

#! 9.5
(define (prepend-every pre-wd sent)
  (every (lambda (wd) (word pre-wd wd)) sent))

(prepend-every 's '(he aid he aid))
(prepend-every 'anti '(dote pasto gone body))

#! 9.6
(define (square x) (* x x))
(define (sentence-version fn)
  (lambda (sent) (every fn sent)))
((sentence-version first) '(if i fell))
((sentence-version square) '(8 2 4 6))

#! 9.7
(define (letterwords letter sent)
  (keep (lambda (wd) (member? letter wd)) sent))

(letterwords 'o '(got to get you into my life))

#! 9.8
(define (hang secret-word guessed-letters)
  (every (lambda (letter) (if (member? letter guessed-letters)
                              letter
                              '_)) secret-word))
(hang 'potsticker 'etaoi)
(hang 'potsticker 'etaoip)
(hang 'potsticker 'etaoips)
(hang 'potsticker 'etaoipsckr)

#! 9.9
(define (common-words sent-a sent-b)
  (keep (lambda (a-word) (member? a-word sent-b)) sent-a))

(common-words '(words in a ball fun toast) '(words in b toast jam butter))

#! 9.10
;; TODO: Could make this more robust for words/sentences, but for now this is for letters in word
(define (appearances2 needle haystack)
  (count (keep (lambda (current) (equal? needle current)) haystack)))
(appearances2 'b 'basketball)

#! 9.11
(define (unabbrev sent-a sent-b)
  (every (lambda (wd) (if (number? wd)
                          (item wd sent-b)
                          wd)) sent-a))
(unabbrev '(john 1 wayne fred 4) '(bill hank kermit joey))

#! 9.12
(define (first-last sent)
  (keep (lambda (wd) (equal? (first wd) (last wd))) sent))
(first-last '(california ohio nebraska alabama alaska massachusetts))

#! 9.13
(define (compose2 f g)
  (lambda (x) (f (g x))))

((compose2 sqrt abs) -25)
(define second2 (compose2 first bf))
(second2 '(higher order function))

#! 9.14
(define (substitute wd-a wd-b sent)
  (every (lambda (wd) (if (equal? wd wd-b)
                          wd-a
                          wd)) sent))

(substitute 'maybe 'yeah '(she loves you yeah yeah yeah))

#! 9.15
(define (type-check f pred)
  (lambda (x)(if (pred x)
                 (f x)
                 #f)))

(define safe-sqrt (type-check sqrt number?))
(safe-sqrt 16)
(safe-sqrt 'sarsaparilla)

#! 9.16
;; APL: Vector -> sentence of numbers here in Simply Scheme
(define (aplize f)
  (lambda (input) (if (word? input)
                      (f input)
                      (every f input))))
(define apl-sqrt (aplize sqrt))
(apl-sqrt 36)
(apl-sqrt '(1 100 25 16))

#! 9.17
;; keep2: write in terms of every and accumulate
(define (keep2 pred sent)
  (accumulate sentence (every (lambda (wd) (if(pred wd)
                                              wd
                                              '())) sent)))
(keep2 (lambda (x) x) '(this is a test))
(keep2 (lambda (x) (> (count x) 2)) '(this is a test))
                  