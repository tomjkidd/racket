#lang racket
(require (planet dyoo/simply-scheme:2))

(define (square x) (* x x))

#! 6.1
#! First true expression will return it's expression
#! (= 3 4) is #f, (< 2 3) is #t, '(nowhere man)
(cond ((= 3 4) '(this boy))
      ((< 2 5) '(nowhere man))
      (else '(two of us)))
#! This one tricked me, 3 is what it evaluates to.
#! Has to do with each condition containing two expressions, empty? is an expression (function) that is 'true',
#! so the second expression, 3, is evaluated.
(cond (empty? 3)
      (square 7)
      (else 9))

#! 6.2
(or #f #f #f #t) ;; #t
(and #f #f #f #t) ;; #f
(or (= 2 3) (= 4 3)) ;; #f
(not #f) ;; #t
(or (not (= 2 3)) (= 4 3)) ;; #t
(or (and (= 2 3) (= 3 3)) (and (< 2 3) (< 3 4))) ;; #t

#! 6.3
(define (sign number)
  (cond ((< number 0) 'negative)
        ((= number 0) 'zero)
        (else 'positive)))

(sign -1)
(sign 0)
(sign 1)

#! 6.4
(define (utensil meal)
  (if (equal? meal 'chinese) 'chopsticks 'fork))

(utensil 'chinese)
(utensil 'indian)

#! 6.5
#|
 1-11, same with am
 ...
 12 -> PM
 13 -> 1PM, -12 with pm
 ...
 24 -> 12 AM
|#
(define (european-time time-am-pm)
  (define hour (first time-am-pm))
  (define am-pm (first (bf time-am-pm)))
  (cond ((equal? am-pm 'am) (if (= hour 12) 24 hour))
        (else (+ hour 12))))

(define (american-time hour)
  (cond ((= hour 24) (sentence 12 'am))
        ((< hour 12) (sentence hour 'am))
        ((= hour 12) (sentence hour 'pm))
        (else (sentence (- hour 12) 'pm))))

(european-time '(8 am))
(european-time '(4 pm))
(american-time 21)
(american-time 12)
(european-time '(12 am))

#! 6.6
(define (teen age)
  (and (<= age 19) (>= age 13)))

(teen 12)
(teen 13)
(teen 19)
(teen 20)

#! 6.7
(define (type-of x)
  (cond ((number? x) 'number)
        ((word? x) 'word)
        ((sentence? x) 'sentence)
        ((boolean? x) 'boolean)))

(type-of '(getting better))
(type-of 'revolution)
(type-of (= 3 3))
(type-of 1)

#! 6.8
(define (indef-article w)
  (if (member? (first w) '(a e i o u))
      (sentence 'an w)
      (sentence 'a w)))

(indef-article 'beatle)
(indef-article 'album)

#! 6.9
#! fry, fries; bird, birds; Come to think of it fish, fishes; deer, deer...
(define (thismany number singular)
  (cond ((<= number 1) (sentence 1 singular))
        ((> number 1) (if (equal? (last singular) 'y)
                          (sentence number (word (bl singular) 'ies))
                          (sentence number (word singular 's))))))

(thismany 1 'partridge)
(thismany 3 'french-hen)
(thismany 1 'fry)
(thismany 2 'fry)

#! 6.10
(define (sort2 2-item-list)
  (define x (first 2-item-list))
  (define y (last 2-item-list))
  (if (< x y) (sentence x y) (sentence y x)))

(sort2 '(5 7))
(sort2 '(7 5))

#! 6.11
#|
30 Sept (9), Apr (4), June (6), Nov (11)
31 rest
28 Feb (2) not divisible by 4, 29 when divisible by 4 except for when divisible by 400 and 100
|#
(define (valid-date? month day year)
  #! assume all years are valid
  (cond ((or (< month 1) (> month 12)) #f)
        ((member? month '(4 6 9 11)) (and (> day 0) (<= day 30)))
        ((equal? month 2) (cond ((= (remainder year 4) 0) (if (= (remainder year 100) 0)
                                                              (and (> day 0) (<= day 29))
                                                              (and (> day 0) (<= day 28))))
                                (else (and (> day 0) (< day 28)))))
        (else (and (> day 0) (<= day 31)))))

(valid-date? 10 4 1949)
(valid-date? 20 4 1776)
(valid-date? 5 0 1992)
(valid-date? 2 29 1990)
(valid-date? 2 29 2000)

#! 6.12
(define (plural w)
  #! TODO: Figure out how to implement this...
  #! This will help simplify things and allow for fish -> fishes
  (define (ends-with word ending)
    #f) ;; ...
    
  (define (vowel? x)
    (member? x '(a e i o u)))
  (define (ends-in-y w)
    (equal? (last w) 'y))
  (define (ends-in-y-preceeded-by-vowel w)
    (and (ends-in-y w) (vowel? (last (bl w)))))
  (cond ((equal? (last w) 'y) (if (ends-in-y-preceeded-by-vowel w)
                                  (word w 's)
                                  (word (bl w) 'ies)))
        ((equal? (last w) 'x) (word w 'es))
        (else (word w 's))))

(plural 'boy)
(plural 'box)
(plural 'fry)

#! 6.13
(define (greet name)
  (cond ((equal? (first name) 'dr) (sentence 'hello 'dr
                                             (cond ((member? (last name) '(jr)) (last (bl name)))
                                                   (else (last name)))))
        ((equal? name '(queen elizabeth)) (sentence '(hello your majesty)))
        ((equal? name '(david livingstone)) (sentence '(dr livingstone i presume?)))
        (else (sentence 'hello (first name)))))

(greet '(john lennon))
(greet '(dr marie curie))
(greet '(dr martin luther king jr))
(greet '(queen elizabeth))
(greet '(david livingstone))

#! 6.14
(define (describe-time seconds)
  (define (minutes-to-seconds minutes)
    (* minutes 60))
  (define (seconds-to-minutes seconds)
    (/ seconds 60))
  #! TODO: provide hours, days, months, years, decade, centuries... or not.
  (cond ((< seconds 60) (sentence seconds 'seconds))
        ((< seconds (minutes-to-seconds 60)) (sentence (seconds-to-minutes seconds) 'minutes))
        (else (sentence seconds 'seconds))))

(describe-time 45)
(describe-time 930)
