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