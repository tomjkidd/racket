#lang racket
(require (planet dyoo/simply-scheme:2))

#|
Hand is 13 cards

A: 4 pts
K: 3 pts
Q: 2 pts
J: 1 pt

Distribution Points:
Exactly 2 cards of a suit: 1 pt
Exactly 1 card of a suit: 2 pts
Exactly 0 cards of a suit: 3 pts

A hand will be represented by a sentence of cards
|#

(define (suit card)
  (first card))

(define (rank card)
  (butfirst card))

(define (card-val card)
  (let ((card-rank (rank card)))
    (cond ((equal? card-rank 'a) 4)
          ((equal? card-rank 'k) 3)
          ((equal? card-rank 'q) 2)
          ((equal? card-rank 'j) 1)
          (else 0))))

(card-val 'cq)
(card-val 's7)
(card-val 'ha)

(define (high-card-points hand)
  (accumulate + (every card-val hand)))
(high-card-points '(sa s10 hq ck c4))
(high-card-points '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3))

(define (count-suit target-suit hand)
  (count (keep (lambda (card) (equal? target-suit (suit card))) hand)))

(count-suit 's '(sa s10 hq ck c4))
(count-suit 'c '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3))
(count-suit 'd '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2))

(define (suit-counts hand)
  (sentence (count-suit 's hand)
            (count-suit 'h hand)
            (count-suit 'c hand)
            (count-suit 'd hand)))

(suit-counts '(sa s10 hq ck c4))
(suit-counts '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3))
(suit-counts '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2))

(define (suit-dist-points suit-card-count)
  (cond ((= suit-card-count 0) 3)
        ((= suit-card-count 1) 2)
        ((= suit-card-count 2) 1)
        (else 0)))

(suit-dist-points 2)
(suit-dist-points 7)
(suit-dist-points 0)

(define (hand-dist-points hand)
  (accumulate + (every (lambda (suit-count) (suit-dist-points suit-count)) (suit-counts hand))))

(hand-dist-points '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3))
(hand-dist-points '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2))

(define (bridge-val hand)
  (+ (high-card-points hand) (hand-dist-points hand)))

(bridge-val '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3))
(bridge-val '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2))