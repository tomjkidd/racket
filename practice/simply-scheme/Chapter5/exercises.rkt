#lang racket
(require (planet dyoo/simply-scheme:2))

#! quote is a special form
(quote square)
(quote (tomorrow never knows))

#! another way to use quote
'square
'(old brown shoe)

(first 'something)
(first '(eight days a week))

(last 'something)
(last '(eight days a week))

(last 910)
(butfirst 'something)
(butfirst '(eight days a week))
(butlast 'something)
(butlast '(eight days a week))
(butlast 910)

(word 'ses 'qui 'pe 'da 'lian 'ism)
(sentence 'carry 'that 'weight)

#! Abbreviations se:sentence bf:butfirst bl:butlast

#! 5.2
(define (f1 a b)
  (bl (bf (sentence a b))))
(f1 '(a b c) '(d e f))

(define (f2 a b)
  (sentence (f1 a b) (word (first a) (last b))))
(f2 '(a b c) '(d e f))

(define (f3 a b)
  (sentence a a))
(f3 '(a b c) '(d e f))

(define (f4 a b)
  (word (first (bf a)) (first (bf b))))
(f4 '(a b c) '(d e f))

#! 5.3
(first 'mezzanine) ;; 'm, takes first letter
(first '(mezzanine)) ;; 'mezzanine, takes first word

#! 5.4
(define (square x) (* x x))
(first (square 7)) ;; 4, takes the first letter of the result 49
(first '(square 7)) ;; 49, takes the first word

#! 5.5
(word 'a 'b 'c) ;; 'abc, creates a word
(se 'a 'b 'c) ;; '(a b c), creates a sentence

#! 5.6
(bf 'zabadak) ;; Uses shorthand name for butfirst
(butfirst 'zabadak) ;; Uses regular definition for butfirst

#! 5.7
(bf 'x) ;; "", Empty word because rest of word is empty
(butfirst '(x)) ;; '() Empty sentence because there are not more words

#! 5.8
#! (here, there and everywhere) ;; Nope
#! (help!) ;; Nope
#! (all i've got to do) ;; Nope
#! (you know my name (look up the number)) ;; Nope

#! 5.9
