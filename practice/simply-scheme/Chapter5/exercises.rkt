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
(se (word (bl (bl (first '(make a)))) ;; 'ma
          (bf (bf (last '(baseball mitt))))) ;; 'tt
    (word (first 'with) (bl (bl (bl (bl 'rigidly)))) ;; 'wrig
          (first 'held) (first (bf 'stitches)))) ;; 'ht -> '(matt wright)

(se (word (bl (bl 'bring)) 'a (last 'clean)) ;; 'brian
    (word (bl (last '(baseball hat))) (last 'for) (bl (bl 'very)) ;; 'harve
          (last (first '(sunny days))))) ;; y -> '(brian harvey)

#! 5.10
(bf 'wright)
(bf '(this is a sentence that returns a sentence))

#! 5.11
(last '(this is a sentence where last is a word))
(last '('(last here) '(is a sentence)))

#! 5.12
;;(first '()) ;; Error
;;(first "") ;; Error

;;(last '()) ;; Error
;;(last "") ;; Error
(last 'a) ;; 'a 
(last '(a)) ;; 'a

;;(bf '()) ;; Error
;;(bf "") ;; Error
(bf 'a) ;; ""
(bf '(a)) ;; '()

;;(bl '()) ;; Error
;;(bl "") ;; Error
(bl 'a) ;; ""
(bl '(a)) ;; '()

#! 5.13
;; ' 'banana stands for quote 'banana, without application
(first ' 'banana) ;; 'quote, the first argument

#! 5.14
(define (third x)
  (first (bf (bf x))))

(third 'abc)
(third '(word1 word2 word3))

#! 5.15
(define (first-two x)
  (word (first x)
        (first (bf x))))

(first-two 'amulatory)

#! 5.16
(define (two-first x y)
  (word (first x) (first y)))

(two-first 'brain 'epstein)

(define (two-first-sent x)
  (word (first (first x))
        (first (first (bf x)))))

(two-first-sent '(brian epstein))

#! 5.17
(define (knight name)
  (sentence 'sir
        (first name)
        (first (bf name))))

(knight '(david wessel))

#! 5.18
(define (ends word)
  (word (first word) (last word)))

;; (ends 'john) ;; Error, using word as an argument makes it so that the word function is clobbered in the body.

#! 5.19
(define (insert-and items)
  (sentence (bl items)
            'and
            (last items)))

(insert-and '(john bill wayne fred joey))

#! 5.20
(define (middle-names fullname)
  (bl (bf fullname)))

(middle-names '(james paul mccartney))
(middle-names '(john ronald raoul tolkien))
(middle-names '(bugs bunny))
(middle-names '(peter plair denis bernard noone))

#! 5.21
(define (query statement)
  (sentence (first (bf statement))
            (first statement)
            (bl (bf (bf statement)))
            (word (last statement) '?)))

(query '(you are experienced))
(query '(i should have known better))