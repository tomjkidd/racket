#lang racket

(require "../core.rkt")

(lat? '(Jack Sprat could eat no chicken fat))
(lat? (list '(Jack) 'Sprat 'could 'eat 'no 'chicken 'fat))
(lat? (list 'Jack '(Sprat could) 'eat 'no 'chicken 'fat))
(lat? '())

(lat? '(bacon and eggs))

(or (null? '()) '(d e f g))
(or (null? '(a b c)) (null? '()))
(or (null? '(a b c)) (null? '(atom)))

(member? 'tea '(coffee tea or milk))
(member? 'poached '(fried eggs and scrambled eggs))
(member? 'meat '(mashed potatoes and meat gravy))

#|
The First Commandment
Always ask null? as the first question in expressing any function.
|#
(member? 'meat '(meat gravy))
(member? 'meat '(and meat gravy))
(member? 'liver '(bagels and lox))