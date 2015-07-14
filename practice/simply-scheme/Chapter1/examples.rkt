#lang racket
(require (planet dyoo/simply-scheme))

(define (acronym phrase)
  (accumulate word (every first phrase)))

(define (acronym-2 phrase)
  (accumulate word (every first (keep real-word? phrase))))
(define (real-word? wd)
  (not (member? wd '(a the an in of and for to with))))

(define (pigl wd)
  (if (member? (first wd) 'aeiou)
      (word wd 'ay)
      (pigl (word (butfirst wd) (first wd)))))

(define (rotate wd)
  (word (butfirst wd) (first wd)))

#! every will call pigl on each word
(every pigl '(the ballad of john and yoko))

(define (choices menu)
  (if (null? menu)
      '(())
      (let ((smaller (choices (cdr menu))))
        (reduce append
                (map (lambda (item) (prepend-every item smaller))
                     (car menu))))))
(define (prepend-every item lst)
  (map (lambda (choice) (se item choice)) lst))

(choices '((small medium large)
           (vanilla (ultra chocolate) (rum raisin) ginger)
           (cone cup)))

(define (combinations size set)
  (cond ((= size 0) '(()))
        ((empty? set) '())
        (else (append (prepend-every (first set)
                                     (combinations (- size 1)
                                                   (butfirst set)))
                      (combinations size (butfirst set))))))
(combinations 3 '(a b c d e))

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(factorial 1000)