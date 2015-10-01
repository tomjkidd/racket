#lang racket

(require (planet dyoo/simply-scheme:2))
(require racket/trace)
(require "core.rkt"
         "db-core.rkt")

(provide current-db
         new-db
         insert
         db-insert
         get-record
         ask
         count-db
         list-db

         display-field
         display-record

         record-index-valid?
         edit-record)

;; Current Database (state)
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
      (begin
        (display "listed")
        (newline))
      (begin
        (display-record (car records) fields record-index)
        (display-record-helper (cdr records) fields (+ record-index 1)))))
              
(define (list-db)
  (display-record-helper (reverse (db-records (current-db))) (db-fields (current-db)) 1))

(define record-index-valid?
  (lambda (index)
    (one-based-index-valid? (db-records (current-db)) index)))

(define edit-record
  (lambda (index)
    (cond ((record-index-valid? index)
           (display-record (list-ref (reverse (db-records (current-db))) (- index 1))
                           (db-fields (current-db))
                           index)                           
           (display "Edit which field?"))
          (else (display index)
                (display " is not a valid record")
                (newline)))))