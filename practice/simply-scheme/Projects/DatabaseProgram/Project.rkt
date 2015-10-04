#lang racket

(require (planet dyoo/simply-scheme:2))
(require racket/trace)
(require "db.rkt")

(new-db "test" '("Column A" "Column B"))
(db-insert (vector 1 2) (current-db))
(db-insert (vector 3 4) (current-db))
(db-insert (vector 7 8) (current-db))

#|(count-db)
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
(newline)|#

#|
NOTE: This is a small script of input to test the basic edit functionality.
(edit-record 2)
"Column A"
"bees"
#f
|#

#|(save-db)
(load-db "test")
(list-db)
(count-db)|#