#lang racket

(require (planet dyoo/simply-scheme:2))
(require racket/trace)
(require "core.rkt")
(require "db.rkt")

#|(new-db "test" '("Column A" "Column B"))
(db-insert (vector 1 2) (current-db))
(db-insert (vector 3 4) (current-db))
(db-insert (vector 7 8) (current-db))|#

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

(new-db "albums" '(artist title year brian-likes?))
(db-insert (vector '(the beatles) "A Hard Day's Night" 1964 #t) (current-db))
(db-insert (vector '(the zombies) "Odessey and Oracle" 1967 #t) (current-db))
(db-insert (vector '(frank zappa) "Hot Rats" 1970 #f) (current-db))
(db-insert (vector '(the beatles) "Rubber Soul" 1965 #t) (current-db))
(db-insert (vector '(the bill frisell band) "Where in the world?" 1991 #f) (current-db))

(list-db)

(define blank (blank-record))
(record-set! "artist" blank "Led Zeppelin")
(record-set! "title" blank "IV")
(record-set! "year" blank "1971")
(record-set! "brian-likes?" blank #f)

blank

(db-insert blank (current-db))

(list-db)

#|(sort-mod (lambda (r1 r2) (< r1 r2)) '(2 4 3 5 1))
(sort-mod (lambda (r1 r2) (string<? r1 r2)) '("A" "C" "E" "D" "F" "B"))

(sort-db (lambda (r1 r2) (before? (get 'title r1) (get 'title r2))))
(list-db)

(sort-db (lambda (r1 r2) (< (get 'year r1) (get 'year r2))))
(list-db)|#

(display "Sorted by title using sort-on-by")
(newline)
(sort-on-by 'title before?)
(list-db)
(newline)

(display "Sorted by year using sort-on-by")
(newline)
(sort-on-by 'year <)
(list-db)
