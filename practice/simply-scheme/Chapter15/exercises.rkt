#lang racket
(require (planet dyoo/simply-scheme:2))
(require racket/trace)

(before? 'a 'b)

(define (earliest-helper so-far rest)
  (cond ((empty? rest) so-far)
        ((before? so-far (first rest))
         (earliest-helper so-far (bf rest)))
        (else (earliest-helper (first rest) (bf rest)))))

(define (earliest-word sent)
  (earliest-helper (first sent) (bf sent)))

(earliest-word '(z x y c b a))

(define (remove-once wd sent)
  (cond ((empty? sent) '())
	((equal? wd (first sent)) (bf sent))
	(else (se (first sent) (remove-once wd (bf sent))))))

(define (sort-mod sent)
  (if (empty? sent)
      '()
      (se (earliest-word sent)
          (sort-mod (remove-once (earliest-word sent) sent)))))

(sort-mod '(the whole world loves it when you are in the news))

(define (from-binary bits)
  (if (empty? bits)
      0
      (+ (* (first bits) (expt 2 (count (bf bits))))
         (from-binary (bf bits)))))

(define (from-binary-efficient bits)
  (if (empty? bits)
      0
      (+ (* (from-binary (bl bits)) 2)
         (last bits))))

(from-binary 1101)
(every from-binary '(0 1 10 11 100 101 110 111 1000))
(every from-binary-efficient '(0 1 10 11 100 101 110 111 1000))

(define (merge sent-a sent-b)
  (cond ((empty? sent-a) sent-b)
        ((empty? sent-b) sent-a)
        (else (let ((first-a (first sent-a))
                    (first-b (first sent-b)))
                (if (before? first-a first-b)
                    ;; Take from a, and merge (butfirst a) with b
                    (se first-a
                        (merge (bf sent-a)
                               sent-b))
                    ;; Take from b, and merge (butfirst b) with a
                    (se first-b
                        (merge sent-a
                               (bf sent-b))))))))

(define (one-half sent)
  (if (<= (count sent) 1)
      sent
      (se (first sent) (one-half (bf (bf sent))))))

(define (other-half sent)
  (if (<= (count sent) 1)
      '()
      (se (first (bf sent)) (other-half (bf (bf sent))))))

(define (mergesort sent)
  (if (<= (count sent) 1)
      sent
      (merge (mergesort (one-half sent))
             (mergesort (other-half sent)))))

(mergesort '(that is the way that I like it all to see bugs kiss pow))

#|
rat -> '("" r a t ra rt at rat)
brat -> (se (subsets rat) '(b br ba bt bra brt bat brat)) 
|#

(define (prepend-every letter sent)
  (if (empty? sent)
      '()
      (se (word letter (first sent))
          (prepend-every letter (bf sent)))))

(define (subsets wd)
  (if (empty? wd)
      (se "")
      (let ((smaller (subsets (bf wd))))
        (se smaller
            (prepend-every (first wd) smaller)))))

(prepend-every 'b (subsets 'rat))
(subsets 'brat)

#! 15.1
(define (max-power-of-two index num)
  (if (>= (- (expt 2 index) 1) num)
      index
      (max-power-of-two (+ index 1) num)))

(trace max-power-of-two)
(max-power-of-two 0 64)

(define (to-binary-helper index num)
  (cond ((< index 0) "")
        (else (let ((remaining (remainder num (expt 2 index))))
          (word (quotient num (expt 2 index))
              (to-binary-helper (- index 1) remaining))))))

(define (to-binary num)
  (to-binary-helper (- (max-power-of-two 0 num) 1) num))

(trace to-binary-helper)
(every to-binary '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
(to-binary 9)
(to-binary 23)

#! 15.2
#| The idea for solving this one is to first combine all the words together,
and then to check that the resulting large word is a palindrome.
|#
(define (palindrome-word? wd)
  (cond ((empty? wd) #t)
        ((= (count wd) 1) #t)
        (else (and (equal? (first wd) (last wd)) (palindrome-word? (bl (bf wd)))))))

(define (palindrome? sent)
  (palindrome-word? (accumulate word sent)))

(palindrome-word? 'amanaplanacanalpanama)
(palindrome-word? 'abc)

(palindrome? '(flee to me remote elf))
(palindrome? '(flee to me remote control))

#! 15.3
#|
bat -> b ba bat at t
|#
(define (substrings-helper wd)
  (cond ((empty? wd) wd)
        ((= (count wd) 1) wd)
        (else (se (substrings-helper (bf wd))
                  wd
                  (substrings-helper (bl wd))))))

(define (remdup-rec encountered sent)
  (cond ((empty? sent) encountered)
        ((member? (first sent) encountered) (remdup-rec encountered (bf sent)))
        (else (remdup-rec (se encountered (first sent)) (bf sent)))))
           
(define (remdup sent)
  (remdup-rec '() sent))

(define (substrings wd)
  (remdup (substrings-helper wd)))

(substrings 'bat)

#! 15.4
(define (substring? needle haystack)
  (member? needle (substrings haystack)))

(substring? 'ssip 'mississippi)
(substring? 'misip 'mississippi)
