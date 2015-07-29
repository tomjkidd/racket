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