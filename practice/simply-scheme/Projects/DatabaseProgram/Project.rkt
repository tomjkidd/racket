#lang racket

(require (planet dyoo/simply-scheme:2))
(require racket/trace)
(require "db.rkt")

(new-db "test" '("Column A" "Column B"))
(db-insert #(1 2) (current-db))
(db-insert #(3 4) (current-db))
(db-insert #(5 6) (current-db))

(count-db)
(list-db)

(define record-check
  (lambda (index)
    (cond ((record-index-valid? index)
           (display index)
           (display " is a valid record")
           (newline))
          (else 
           (display index)
           (display " is not a valid record")
           (newline)))))

(record-check 0)
(record-check 1)
(record-check 2)
(record-check 3)
(record-check 4)
(newline)

(edit-record 2)