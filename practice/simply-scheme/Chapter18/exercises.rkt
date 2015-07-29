#lang racket
(require (planet dyoo/simply-scheme:2))
(require racket/trace)

(define (leaf datum)
  (make-node datum '()))

(define (cities name-list)
  (map leaf name-list))

(define world-tree
  (make-node 'world
             (list (make-node 'italy
                              (cities '(venezia riomaggiore firenze roma)))
                   (make-node '(united states)
                              (list (make-node 'california
                                               (cities '(berkeley (san francisco) gilroy)))
                                    (make-node 'massachusetts
                                               (cities '(cambridge amherst sudbury)))
                                    (make-node 'ohio (cities '(kent)))))
                   (make-node 'zimbabwe (cities '(harare hwange)))
                   (make-node 'china
                              (cities '(beijing shanghai guangzhou suzhou)))
                   (make-node '(great britain)
                              (list (make-node 'england (cities '(liverpool)))
                                    (make-node 'scotland
                                               (cities '(edinburgh glasgow (gretna green))))
                                    (make-node 'wales
                                               (cities '(abergavenny)))))
                   (make-node 'australia
                              (list (make-node 'victoria (cities '(melbourne)))
                                    (make-node '(new south wales) (cities '(sydney)))
                                    (make-node 'queensland
                                               (cities '(cairns (port douglas))))))
                   (make-node 'honduras (cities '(tegucigalpa))))))

(define (count-leaves-first-version tree)
  (if (leaf? tree)
      1
      (reduce + (map count-leaves (children tree)))))

(define (count-leaves tree)
  (if (leaf? tree)
      1
      (count-leaves-in-forest (children tree))))

(define (count-leaves-in-forest forest)
  (if (null? forest)
      0
      (+ (count-leaves (car forest))
         (count-leaves-in-forest (cdr forest)))))

(define (leaf? node)
  (null? (children node)))

(count-leaves world-tree)

(define (in-tree? place tree)
  (or (equal? place (datum tree))
      (in-forest? place (children tree))))

(define (in-forest? place forest)
  (if (null? forest)
      #f
      (or (in-tree? place (car forest))
          (in-forest? place (cdr forest)))))

(in-tree? 'abergavenny world-tree)
(in-tree? 'abbenay world-tree)
(in-tree? 'venezia (cadr (children world-tree)))

(define (locate city tree)
  (if (equal? city (datum tree))
      (list city)
      (let ((subpath (locate-in-forest city (children tree))))
        (if subpath
            (cons (datum tree) subpath)
            #f))))

(define (locate-in-forest city forest)
  (if (null? forest)
      #f
      (or (locate city (car forest))
          (locate-in-forest city (cdr forest)))))

(locate 'berkeley world-tree)

world-tree

(car (children world-tree))

#|
Each element is:
 1. A number
 2. member of {+,-,*,/}
 3. A sublist satisfying 1. 2. or 3.
|#
(define (parse expr)
  (parse-helper expr '() '()))

(define (parse-helper expr operators operands)
  (cond ((null? expr)
         (if (null? operators)
             (car operands)
             (handle-op '() operators operands)))
        ((number? (car expr))
         (parse-helper (cdr expr)
                       operators
                       (cons (make-node (car expr) '()) operands)))
        ((list? (car expr))
         (parse-helper (cdr expr)
                       operators
                       (cons (parse (car expr)) operands)))
        (else (if (or (null? operators)
                      (> (precedence (car expr))
                         (precedence (car operators))))
                  (parse-helper (cdr expr)
                                (cons (car expr) operators)
                                operands)
                  (handle-op expr operators operands)))))

(define (handle-op expr operators operands)
  (parse-helper expr
                (cdr operators)
                (cons (make-node (car operators)
                                 (list (cadr operands) (car operands)))
                      (cddr operands))))

(define (precedence oper)
  (if (member? oper '(+ -)) 1 2))

(define (compute tree)
  (if (number? (datum tree))
      (datum tree)
      ((function-named-by (datum tree))
       (compute (car (children tree)))
       (compute (cadr (children tree))))))

(define (function-named-by oper)
  (cond ((equal? oper '+) +)
        ((equal? oper '-) -)
        ((equal? oper '*) *)
        ((equal? oper '/) /)
        (else (error "no such operator as" oper))))

(compute (parse '(4 + 3 * 7 - 5 / (3 + 4) + 6)))
