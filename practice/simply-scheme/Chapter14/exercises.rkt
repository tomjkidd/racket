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
