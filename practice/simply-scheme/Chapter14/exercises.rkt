#lang racket
(require (planet dyoo/simply-scheme:2))
(require racket/trace)

#! 14.1
;; keep-like
(define (remove-once wd sent)
  (let ((first-wd (first sent)))
    (if (equal? wd first-wd)
        (bf sent)
        (se first-wd
            (remove-once wd (bf sent))))))

(trace remove-once)
(remove-once 'morning '(good morning good morning))

#! 14.2
;; every-like
(define (up wd)
  (if (empty? wd)
      '()
      (se (up (bl wd))
          wd)))
(trace up)
(up 'town)

#! 14.3
(define (remdup-rec encountered sent)
  (cond ((empty? sent) encountered)
        ((member? (first sent) encountered) (remdup-rec encountered (bf sent)))
        (else (remdup-rec (se encountered (first sent)) (bf sent)))))
           
(define (remdup sent)
  (remdup-rec '() sent))
(trace remdup-rec)
(remdup '(ob la di ob la da))

#! 14.4
(define (odds sent)
  (cond ((empty? sent) '())
        ((= (count sent) 1) sent)
        ((>= (count sent) 2) (se (first sent) (odds (bf (bf sent)))))))

(odds '(i lost my little girl))

(define (evens sent)
  (if (empty? sent)
      '()
      (odds (bf sent))))

(evens '(i lost my little girl))

#! 14.5
(define (letter-count sent)
  (accumulate + (every (lambda (wd) (count wd)) sent)))

(letter-count '(fixing a hole))

#! 14.6
(define (member-mod? needle haystack)
  (cond ((empty? haystack) #f)
        (else (or (equal? needle (first haystack))
            (member-mod? needle (bf haystack))))))

(member-mod? 1 '(1 2 3))
(member-mod? 1 '(2 3 4))
(member-mod? 'c '(a b c))
(member-mod? 'd '(a b c))

#! 14.7
(define (differences sent)
  (cond ((>= (count sent) 2) (let ((first-num (first sent))
                                   (second-num (first (bf sent))))
                               (se (- second-num first-num)
                                   (differences (bf sent)))))
        (else '())))

(trace differences)
(differences '(4 23 9 87 6 12))

#! 14.8
(define (expand-mod sent)
  (if (empty? sent)
      '()
      (if (>= (count sent) 2)
          (if (number? (first sent))
              (se (repeat (first sent) (first (bf sent)))
                  (expand-mod (bf sent)))
              (se (first sent)
                  (expand-mod (bf sent))))
          sent)))
          
(define (repeat num-times wd)
  (if (> num-times 1)
      (se wd
          (repeat (- num-times 1) wd))
      '()))

(trace expand-mod)
(expand-mod '(4 calling birds 3 french hens))
(expand-mod '(the 7 samurai))

#! 14.9
(define (location-helper index wd sent)
  (if (empty? sent)
      #f
      (if (equal? wd (first sent))
          index
          (location-helper (+ index 1) wd (bf sent)))))

(define (location wd sent)
  (location-helper 1 wd sent))

(location 'you '(you never give me your money))
(location 'never '(you never give me your money))
(location 'give '(you never give me your money))
(location 'me '(you never give me your money))
(location 'your '(you never give me your money))
(location 'money '(you never give me your money))
(location 'bull '(you never give me your money))

#! 14.10
(define (count-adjacent-duplicates sent)
  (cond ((< (count sent) 2) 0)
        ((>= (count sent) 2) (if (equal? (first sent) (first (bf sent)))
                                 (+ 1 (count-adjacent-duplicates (bf sent)))
                                 (count-adjacent-duplicates (bf sent))))))

(trace count-adjacent-duplicates)
(count-adjacent-duplicates '(y a b b a d a b b a d o o))
(count-adjacent-duplicates '(yeah yeah yeah))

#! 14.11
(define (remove-adjacent-duplicates sent)
  (cond ((< (count sent) 0) '())
        ((= (count sent) 1) sent)
        ((>= (count sent) 2) (if (equal? (first sent) (first (bf sent)))
                                 (remove-adjacent-duplicates (bf sent))
                                 (se (first sent) (remove-adjacent-duplicates (bf sent)))))))

(trace remove-adjacent-duplicates)
(remove-adjacent-duplicates '(y a b b a d a b b a d o o))
(remove-adjacent-duplicates '(yeah yeah yeah))

#! 14.12
(define (square x) (* x x))
(define (progressive-squares? sent)
  (cond ((<= (count sent) 1) #t)
        ((> (count sent) 2) (if (equal? (square (first sent)) (first (bf sent)))
                                #t
                                #f))))
(progressive-squares? '(3 9 81 6561))
(progressive-squares? '(25 36 49 64))