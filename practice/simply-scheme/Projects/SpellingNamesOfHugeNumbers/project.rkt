#lang racket
(require (planet dyoo/simply-scheme:2))
(require racket/trace)

#|
Used Short Scale: https://en.wikipedia.org/wiki/Names_of_large_numbers
Naming
0 -> ""
1 -> 'one
2 -> 'two
3 -> 'three
4 -> 'four
5 -> 'five
6 -> 'six
7 -> 'seven
8 -> 'eight
9 -> 'nine
10 -> 'ten
11 -> 'eleven
12 -> 'twelve
13 -> 'thirteen
14 -> 'fourteen
15 -> 'fifteen
16 -> 'sixteen
17 -> 'seventeen
18 -> 'eighteen
19 -> 'nineteen
20 -> 'twenty
21-29 -> '(name-tens name-ones)
30 -> 'thirty
31-39 -> '(name-tens name-ones)
...
100 -> (name-hundreds name-tens name-ones)
110 -> (name-hundreds name-special)
|#
(define digit-names '("" one two three four five six seven eight nine))
(define tens-names '("" twenty thirty fourty fifty sixty seventy eight ninety))

(define (name-ones digit)
  (item (+ 1 digit) digit-names))

(define (name-between-ten-and-teens num)
  (if (and (number? num) (> num 9) (< num 20))
      (cond ((= num 10) 'ten)
            ((= num 11) 'eleven)
            ((= num 12) 'twelve)
            ((= num 13) 'thirteen)
            ((= num 14) 'fourteen)
            ((= num 15) 'fifteen)
            ((= num 16) 'sixteen)
            ((= num 17) 'seventeen)
            ((= num 18) 'eighteen)
            ((= num 19) 'nineteen)
            (else ""))
      ""))

(define (name-tens num)
  (if (number? num)
      (cond ((and (> num 9) (< num 20)) (name-between-ten-and-teens num))
            ((= num 0) "")
            (else (se (item (first num) tens-names)
                        (name-ones (bf num)))))
      ""))

(define (name-between-twenty-and-nineninetynine num)
  (if (number? num)
      (cond ((= (count num) 3) (se (name-ones (first num)) 'hundred
                                   (name-tens (bf num))))
            ((= (count num) 2) (name-tens num))
            ((= (count num) 1) (name-ones num))
            (else ""))
      ""))

(define scale-names
  '(thousand million billion trillion quadrillion quintillion
             sextillion septillion octillion nonillion decillion))

(define (groups-of-three wd)
  (cond ((>= (count wd) 3)
         (se (groups-of-three (bl (bl (bl wd))))
             (word (last (bl (bl wd))) (last (bl wd)) (last wd))))
        ((empty? wd) '())
        (else (se wd))))

(define (name-triple-filter triple)
  ;; Remove leading zeros and non-numbers
  (cond ((equal? triple "") "")
        ((and (= (count triple) 1) (equal? triple 0)) "")
        ((or (member? (first triple) '(0 "")) (not (number? (first triple)))) name-triple-filter (bf triple))
        (else triple)))

(define (get-triple-name num)
  ;; Assumes num to be between 0 and 999
  (if (number? num)
      (cond ((and (>= num 0) (< num 10)) (name-ones num))
            ((and (>= num 10) (< num 20)) (name-between-ten-and-teens num))
            ((and (>= num 20) (< num 999)) (name-between-twenty-and-nineninetynine num)) 
            (else ""))
      ""))
              
(define (name-triple triple)
  (get-triple-name (name-triple-filter triple)))

(define (add-triple-helper index triples)
  (if (empty? triples)
      '()
      (se (name-triple (first triples))
          (if (> index 0)
              (item index scale-names)
              '())
          (if (> index 0)
              (add-triple-helper (- index 1) (bf triples))
              '()))))

(define (number-name num)
  (let ((triples (groups-of-three num)))
    (add-triple-helper (- (count triples) 1) triples)))

#| First attempt.
(define (add-names-rec index triples)
  (cond ((empty? triples) '())
        (else (se (add-names-rec (+ 1 index) (bl triples))
                  (last triples)
                  (cond ((= index 0) '())
                        ((> index (count scale-names)) '())
                        (else (item index scale-names)))))))

;; TODO Remove this, first attempt;
(define (add-names triples)
  (add-names-rec 0 triples))

;; TODO Remove this, first attempt
(define (number-name-first-attempt num)
  (add-names (every name-triple (groups-of-three num))))

(add-names (groups-of-three 5513345))
(add-names (every name-triple (groups-of-three 1428425)))
|#

;; Tests
(every name-ones '0123456789)
(every name-between-ten-and-teens '(9 10 11 12 13 14 15 16 17 18 19 20))

(groups-of-three 5513345)
(groups-of-three 1)
(groups-of-three 12)
(groups-of-three 123)
(groups-of-three 1234)

(name-triple-filter 001)
(name-triple-filter 01)
(name-triple-filter 010)
(name-triple-filter 'a012)

(every name-triple '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19))
(name-triple 100)
(name-triple 20)
(name-triple 001)

(trace add-triple-helper)
(trace name-triple)

(number-name 5513345)