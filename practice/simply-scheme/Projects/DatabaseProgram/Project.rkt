#lang racket
(require (planet dyoo/simply-scheme:2))
(require racket/trace)

;; The database ADT: a filename, list of fields, and list of records

;; Note vector is like make-vector, except that it uses argument directly to set elements.

(define (make-db filename fields records)
  (vector filename fields records))

(define (db-filename db)
  (vector-ref db 0))

(define (db-set-filename! db filename)
  (vector-set! db 0 filename))

(define (db-fields db)
  (vector-ref db 1))

(define (db-set-fields! db fields)
  (vector-set! db 1 fields))

(define (db-records db)
  (vector-ref db 2))

(define (db-set-records! db records)
  (vector-set! db 2 records))

;; Current Database
(define current-state (vector #f))

(define (no-db?)
  (not (vector-ref current-state 0)))

(define (current-db)
  (if (no-db?)
      (error "No current database!")
      (vector-ref current-state 0)))

(define (set-current-db! db)
  (vector-set! current-state 0 db))

(define (current-fields)
  (db-fields (current-db)))

;; Public interface
(define (new-db filename fields)
  (set-current-db! (make-db filename fields '())))

(define (insert)
  (let ((new-record (get-record)))
    (db-insert new-record (current-db)))
  (if (ask "Insert another? ")
      (insert)
      'inserted))

(define (db-insert record db)
  (db-set-records! db (cons record (db-records db))))

(define (get-record)
  (get-record-loop 0
                   (make-vector (length (current-fields)))
                   (current-fields)))

(define (get-record-loop which-field record fields)
  (if (null? fields)
      record
      (begin (display "Value for ")
             (display (car fields))
             (display "--> ")
             (vector-set! record which-field (read))
             (get-record-loop (+ which-field 1) record (cdr fields)))))

(define (ask question)
  (display question)
  (let ((answer (read)))
    (cond ((equal? (first answer) 'y) #t)
          ((equal? (first answer) 'n) #f)
          (else (show "Please type Y or N.")
                (ask question)))))

(define (count-db)
  (length (db-records (current-db))))

(define (for-each-with-index fn lst)
  (for-each-with-index-helper lst fn 0))

(define (for-each-with-index-helper lst fn index)
  (if (null? lst)
      void
      (begin
        (fn (car lst) index)
        (for-each-with-index-helper (cdr lst) fn (+ index 1)))))

(define (display-field field-name field-value)
  (display field-name)
  (display ": ")
  (display field-value))

(define (display-record record fields record-index)
  (display "Record ")
  (display record-index)
  (newline)
  (for-each-with-index (lambda (field index)
                         (display-field field (vector-ref record index))
                         (newline))
                       fields)
  (newline))

(define (display-record-helper records fields record-index)
  (if (null? records)
      (display "listed")
      (begin
        (display-record (car records) fields record-index)
        (display-record-helper (cdr records) fields (+ record-index 1)))))
              
(define (list-db)
  (display-record-helper (db-records (current-db)) (db-fields (current-db)) 1))
              

(define db (make-db "test" '("Column A" "Column B") '( #(1 2) #(3 4))))
(set-current-db! db)
(count-db)
(list-db)