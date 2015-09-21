#lang racket

(require racket/trace)

(provide atom?
         lat?
         
         s-expr?
         count-s-expr
         
         member?
         member*
         rember
         rember*
         multirember
         
         firsts
         insertR
         insertR*
         multiinsertR
         insertL
         insertL*
         multiinsertL
         subst
         subst*
         multisubst
         subst2

         add1
         sub1
         o+
         o-
         o*
         o>
         o<
         o=
         oexpt
         o/
         tup?
         addtup
         tup+

         length

         pick
         rempick

         no-nums
         all-nums

         eqan?
         occur
         occur*

         one?

         leftmost

         eqlist?)

;; An atom is not a pair and not null (the empty list)
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat-book?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define lat?
  (lambda (lst)
    (all-true? (lat-helper lst))))

(define (lat-helper lst)
  (if (null? lst)
      '()
      (cons (atom? (car lst))
            (lat-helper (cdr lst)))))

(define (all-true? bools)
  (foldl (lambda (x result)
           (and x result)) #t bools))

(define s-expr?
  (lambda (x)
    (or (atom? x)
        (list? x))))

(define (count-s-expr lst)
  (count-s-expr-helper lst 0))

(define (count-s-expr-helper lst count)
  (if (null? lst)
      count
      (let ((to-add (if (s-expr? (car lst))
                        1
                        0)))
        (count-s-expr-helper (cdr lst) (+ count to-add)))))

(define member-book?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

(define member?
  (lambda (needle haystack)
    (cond ((null? haystack) #f)
          ((eq? needle (car haystack)) #t)
          (else (member? needle (cdr haystack))))))

;; rmember: Remove first occurence of needle from haystack
(define rember
  (lambda (needle haystack)
    (cond ((null? haystack) '())
          ((equal? needle (car haystack)) (cdr haystack))
          (else (cons (car haystack) (rember needle (cdr haystack)))))))

;; multirember: Remove all occurences of needle from haystack
(define multirember
  (lambda (needle haystack)
    (cond ((null? haystack) '())
          ((eq? needle (car haystack)) (multirember needle (cdr haystack)))
          (else (cons (car haystack) (multirember needle (cdr haystack)))))))

;; firsts: Keep the first from each item in lst
(define firsts
  (lambda (lst)
    (cond ((null? lst) '())
          (else (cons (car (car lst)) (firsts (cdr lst)))))))

;; insertR: insert new to the right of the first occurence of old
(define insertR
  (lambda (new old lst)
    (cond ((null? lst) '())
          ((eq? (car lst) old) (cons old (cons new (cdr lst))))
          (else (cons (car lst) (insertR new old (cdr lst)))))))

;; multiinsertR: insert new to the right of every occurence of old
(define multiinsertR
  (lambda (new old lst)
    (cond ((null? lst) '())
          ((eq? (car lst) old) (cons old (cons new (multiinsertR new old (cdr lst)))))
          (else (cons (car lst) (multiinsertR new old (cdr lst)))))))

;; insertL: insert new to the left of the first occurence of old
;; NOTE: Implementation is same as insertR, but order of cons'ing is different
(define insertL
  (lambda (new old lst)
    (cond ((null? lst) '())
          ((eq? (car lst) old) (cons new (cons old (cdr lst))))
          (else (cons (car lst) (insertR new old (cdr lst)))))))

;; multiinsertL: insert new to the left of every occurence of old
(define multiinsertL
  (lambda (new old lst)
    (cond ((null? lst) '())
          ((eq? (car lst) old) (cons new (cons old (multiinsertL new old (cdr lst)))))
          (else (cons (car lst) (multiinsertL new old (cdr lst)))))))

;; subst: substitute first occurence of old with new
(define subst
  (lambda (new old lst)
    (cond ((null? lst) '())
          ((eq? (car lst) old) (cons new (cdr lst)))
          (else (cons (car lst) (subst new old (cdr lst)))))))

;; multisubst: substitue every occurence of old with new
(define multisubst
  (lambda (new old lst)
    (cond ((null? lst) '())
          ((eq? (car lst) old) (cons new (multisubst new old (cdr lst))))
          (else (cons (car lst) (multisubst new old (cdr lst)))))))

;; subst2: substitue first occurence of either old1 or old2 with new
(define subst2
  (lambda (new old1 old2 lst)
    (cond ((null? lst) '())
          ((or (eq? (car lst) old1) (eq? (car lst) old2)) (cons new (cdr lst)))
          (else (cons (car lst) (subst2 new old1 old2 (cdr lst)))))))
              
;;(trace multirember)

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define o+
  (lambda (n m)
    (cond ((zero? m) n)
          (else (add1 (o+ n (sub1 m)))))))

(define o-
  (lambda (n m)
    (cond ((zero? m) n)
          (else (sub1 (o- n (sub1 m)))))))

(define tup?
  (lambda (lst)
    (cond ((null? lst) #t)
          ((number? (car lst)) (tup? (cdr lst)))
          (else #f))))

(define addtup
  (lambda (tup)
    (cond ((null? tup) 0)
          (else (o+ (car tup) (addtup (cdr tup)))))))

(define o*
  (lambda (n m)
    (cond ((zero? m) 0)
          (else (o+ n (o* n (sub1 m)))))))

#|
Possibilities
(null? tup1) (null? tup2)
#f #f -> return (cons (o+ (car tup1) (car tup2) (tup+ (cdr tup1) (cdr tup2))
#f #t -> return (cons (car tup2) (tup+ tup1 (cdr tup2)))
#t #f -> return (cons (car tup1) (tup+ (cdr tup1) tup2))
#t #t -> return '()
|#
(define tup+
  (lambda (tup1 tup2)
    (cond ((and (null? tup1) (null? tup2)) '())
          ((null? tup1) tup2)
          ((null? tup2) tup1)
          (else (cons (o+ (car tup1) (car tup2))
                      (tup+ (cdr tup1) (cdr tup2)))))))

(define o>
  (lambda (n m)
    (cond ((zero? n) #f)
          ((zero? m) #t)
          (else (o> (sub1 n) (sub1 m))))))

(define o<
  (lambda (n m)
    (cond ((zero? m) #f)
          ((zero? n) #t)
          (else (o< (sub1 n) (sub1 m))))))

(define o=-first
  (lambda (n m)
    (cond ((zero? m) (zero? n))
          ((zero? n) #f)
          (else (o= (sub1 n) (sub1 m))))))

(define o=
  (lambda (n m)
    (cond ((o< n m) #f)
          ((o> n m) #f)
          (else #t))))

(define oexpt
  (lambda (n m)
    (cond ((zero? m) 1)
          (else (o* n (oexpt n (sub1 m)))))))

(define o/
  (lambda (n m)
    (cond ((o< n m) 0)
          (else (add1 (o/ (o- n m) m))))))

(define length
  (lambda (lst)
    (cond ((null? lst) 0)
          (else (add1 (length (cdr lst)))))))

(define pick
  (lambda (n lst)
    (cond ((null? lst) (error "pick ran out of items"))
          ((zero? (sub1 n)) (car lst))
          (else (pick (sub1 n) (cdr lst))))))

(define rempick
  (lambda (n lst)
    (cond ((null? lst) (error "rempick ran out of items"))
          ((one? n) (cdr lst))
          (else (cons (car lst) (rempick (sub1 n) (cdr lst)))))))

(define no-nums
  (lambda (lst)
    (cond ((null? lst) '())
          ((number? (car lst)) (no-nums (cdr lst)))
          (else (cons (car lst) (no-nums (cdr lst)))))))

(define all-nums
  (lambda (lst)
    (cond ((null? lst) '())
          ((number? (car lst)) (cons (car lst) (all-nums (cdr lst))))
          (else (all-nums (cdr lst))))))

;; eqan? : Are the two arguments the same atom?
(define eqan?
  (lambda (a1 a2)
    (cond ((and (number? a1) (number? a2)) (o= a1 a2))
          ((or (number? a1) (number? a2)) #f)
          (else (eq? a1 a2)))))

(define occur
  (lambda (needle haystack)
    (cond ((null? haystack) 0)
          ((eqan? needle (car haystack)) (add1 (occur needle (cdr haystack))))
          (else (occur needle (cdr haystack))))))

(define one?
  (lambda (n)
    (o= n 1)))

(define rember*
  (lambda (needle haystack)
    (cond ((null? haystack) '())
          ((atom? (car haystack))
           (cond ((eqan? needle (car haystack)) (rember* needle (cdr haystack)))
                 (else (cons (car haystack)
                             (rember* needle (cdr haystack))))))
          (else (cons (rember* needle (car haystack))
                      (rember* needle (cdr haystack)))))))

(define insertR*
  (lambda (new old lst)
    (cond ((null? lst) '())
          ((atom? (car lst)) (let ((current (car lst))
                                   (rest (cdr lst)))
                                   (cond ((eq? current old)
                                          (cons current (cons new (insertR* new old rest))))
                                         (else (cons current (insertR* new old rest))))))
          (else (cons (insertR* new old (car lst))
                      (insertR* new old (cdr lst)))))))

(define occur*
  (lambda (needle haystack)
    (cond ((null? haystack) 0)
          ((atom? (car haystack))
           (cond ((eq? needle (car haystack)) (add1 (occur* needle (cdr haystack))))
                 (else (occur* needle (cdr haystack)))))
          (else (o+ (occur* needle (car haystack))
                    (occur* needle (cdr haystack)))))))

(define subst*
  (lambda (new old lst)
    (cond ((null? lst) '())
          ((atom? (car lst))
           (cond ((eqan? (car lst) old)
                  (cons new (subst* new old (cdr lst))))
                 (else
                  (cons (car lst) (subst* new old (cdr lst))))))
          (else (cons (subst* new old (car lst))
                      (subst* new old (cdr lst)))))))

(define insertL*
  (lambda (new old lst)
    (cond ((null? lst) '())
          ((atom? (car lst)) (let ((current (car lst))
                                   (rest (cdr lst)))
                                   (cond ((eq? current old)
                                          (cons new (cons current (insertL* new old rest))))
                                         (else (cons current (insertL* new old rest))))))
          (else (cons (insertL* new old (car lst))
                      (insertL* new old (cdr lst)))))))

(define member*
  (lambda (needle haystack)
    (cond ((null? haystack) #f)
          ((atom? (car haystack))
           (cond ((eq? needle (car haystack)) #t)
                 (else (member* needle (cdr haystack)))))
          (else (or (member* needle (car haystack))
                    (member* needle (cdr haystack)))))))

(define leftmost
  (lambda (lst)
    (cond ((atom? (car lst)) (car lst))
          (else (leftmost (car lst))))))
#|
What are the possibilities for arguments?
l1 can be {null, have an atom at head, have a list at head}
l2 can be {null, have an atom at head, have a list at head}

l1 l2
null null -> #t
null atom -> #f
null list -> #f
atom null -> #f
atom atom -> #t if eqan? for atoms and eqlist? for cdr of lists
atom list -> #f
list null -> #f
list atom -> #f
list list -> #t if eqlist? for car of lists and eqlist for cdr of lists
|#
(define eqlist-1?
  (lambda (l1 l2)
    (cond ((and (null? l1) (null? l2)) #t)
          ((and (null? l1) (atom? (car l2))) #f)
          ((and (null? l1) (list? l2)) #f)
          
          ((and (atom? (car l1)) (null? l2)) #f)
          ((and (atom? (car l1)) (atom? (car l2)))
           (and (eqan? (car l1) (car l2))
                (eqlist? (cdr l1) (cdr l2))))
          ((and (atom? (car l1)) (list? l2)) #f)
          
          ((and (list? (car l1)) (null? l2)) #f)
          ((and (list? (car l1)) (atom? (car l2))) #f)
          ((and (list? (car l2)) (list? (car l2)))
           (and (eqlist? (car l1) (car l2))
                (eqlist? (cdr l1) (cdr l2)))))))

(define eqlist-2?
  (lambda (l1 l2)
    (cond ((and (null? l1) (null? l2)) #t)
          ((or (null? l1) (null? l2)) #f)

          ((and (atom? (car l1)) (atom? (car l2)))
           (and (eqan? (car l1) (car l2))
                (eqlist? (cdr l1) (cdr l2))))
          ((or (atom? (car l1)) (atom? (car l2))) #f)
          
          (else (and (eqlist? (car l1) (car l2))
                     (eqlist? (cdr l1) (cdr l2)))))))

(define eqlist?
  (lambda (l1 l2)
    (cond ((and (null? l1) (null? l2)) #t)
          ((or (null? l1) (null? l2)) #f)
          (else (and (equal? (car l1) (car l2))
                     (eqlist? (cdr l1) (cdr l2)))))))

(define equal?
  (lambda (s1 s2)
    (cond ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
          ((or (atom? s1) (atom? s2)) #f)
          (else (eqlist? s1 s2)))))