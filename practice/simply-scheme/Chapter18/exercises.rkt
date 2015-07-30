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

(parse '(4 + 3 * 7 - 5 / (3 + 4) + 6))

(compute (parse '(4 + 3 * 7 - 5 / (3 + 4) + 6)))

#! 18.1
(locate '(san francisco) world-tree)
;; ((san francisco)) is a list where the first element is a list of two words

#! 18.2
(define (make-node-mod datum children)
  (list datum children))
#|
 If list is used instead of cons:
 datum: you access datum the same way, (car node)
 children: you can no longer access children
 as (cdr node), it would have to be (cadr node)
|#
(define (datum-mod node)
  (car node))

(define (children-mod node)
  (cadr node))

(define node-a (make-node-mod 'datum '(a b c)))

(datum-mod node-a)
(children-mod node-a)

#! 18.3
(define (depth tree)
  (cond ((leaf? tree) 1)
        (else (+ 1 (depth-of-forest (children tree))))))

(define (depth-of-forest forest)
  (cond ((null? forest) 0)
        (else (max (depth (car forest))
                   (depth-of-forest (cdr forest))))))

(trace depth)
(trace depth-of-forest)

(define tree-a (make-node 'a '()))
(define tree-b (make-node 'a (list (make-node 'b '())
                                   (make-node 'c '()))))
(define tree-c (make-node 'a (list (make-node 'b (list (make-node 'd '())))
                                   (make-node 'c '()))))

(depth tree-a)
(depth tree-b)
(depth tree-c)
(untrace depth)
(untrace depth-of-forest)
(depth world-tree)

#! 18.4
(define (count-nodes tree)
  ;; Count all nodes (not just leaves)
  (cond ((null? tree) 0)
        (else (+ 1 (count-nodes-in-forest (children tree))))))

(define (count-nodes-in-forest forest)
  (cond ((null? forest) 0)
        (else (+ (count-nodes (car forest))
                 (count-nodes-in-forest (cdr forest))))))

(count-nodes tree-a)
(count-nodes tree-b)
(count-nodes tree-c)
(count-nodes world-tree)

#! 18.5
(define (prune tree)
  (cond ((leaf? tree) #f)
        (else (make-node (datum tree) (prune-forest (children tree))))))

(define (prune-forest forest)
  (cond ((null? forest) '())
        (else (filter (lambda (child) child) (cons (prune (car forest))
                    (prune-forest (cdr forest)))))))

tree-a
(prune tree-a)

tree-b
(prune tree-b)

tree-c
(prune tree-c)

(prune world-tree)

(datum (caddr (cadr (children world-tree))))
(children (caddr (cadr (children world-tree))))

#! 18.6
(define (operator? x)
  (member? x '(+ - * /)))

(define (parse-scheme expr)
  (parse-scheme-helper expr '() '()))

(define (parse-scheme-helper expr operators operands)
  (cond ((null? expr) (cond ((null? operators) (make-node operands '()))
                            ((not (null? operators)) (make-node (car operators)
                                                                (map (lambda (operand)
                                                                       (parse-scheme operand))
                                                                     (reverse operands))))))
        ((number? expr) (make-node expr '())) ;; Create a node with no children for a number
        ((list? (car expr))
         (parse-scheme-helper (cdr expr) ;; Move a list to the operands list
                              operators
                              (cons (car expr) operands)))
        ((operator? (car expr))
         (parse-scheme-helper (cdr expr) ;; Move an operator to the operators list
                              (cons (car expr) operators)
                              '()))
        ((number? (car expr))
         (parse-scheme-helper (cdr expr) ;; Move a number ot the operands list
                              operators
                              (cons (car expr) operands)))
        (else #f)))

(define (compute-mod tree)
  (if (number? (datum tree))
      (datum tree)
      (apply (function-named-by (datum tree))
             (map compute-mod (children tree)))))
                                  
(define expression-a '(+ 4 3))         
(define expression-b '(* (+ 4 3) 2))
(define expression-c '(+ (* 3 (+ 2 (* 2 1))) (/ (* 8 2) 2)))
(define expression-d '(+ 1 2 3 4))

;;(trace operator?)
;;(trace parse-scheme-helper)
;;(trace parse-scheme)

(compute-mod (parse-scheme expression-a))
(compute-mod (parse-scheme expression-b))
(compute-mod (parse-scheme expression-c))
(compute-mod (parse-scheme expression-d))