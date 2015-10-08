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
         edit-record

         save-db
         load-db

         get
         blank-record
         record-set!

         sort-db
         sort-on-by
         generic-before?)

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

(define (current-records)
  (db-records (current-db)))

;; Public interface
(define (new-db filename fields)
  (clear-current-db!)
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
  (display-record-helper (db-records (current-db)) (db-fields (current-db)) 1))

(define record-index-valid?
  (lambda (index)
    (one-based-index-valid? (db-records (current-db)) index)))

(define field-valid?
  (lambda (field fields)
    (member? field fields)))

(define get-field-index
  (lambda (field fields)
    (get-field-helper field fields 0)))

(define get-field-helper
  (lambda (field fields index)
    (if (equal? field (car fields))
        index
        (get-field-helper field (cdr fields) (+ index 1)))))

(define update-field
  (lambda (record index value)
    (vector-set! record index value)))

(define ask-to-edit
  (lambda (record fields)
    (display "Edit which field?")
    (let ((field (read)))
      (cond ((eq? field #f)
             #f)
            ((field-valid? field fields)
             (begin
               (display "New value for ")
               (display field)
               (display "--> ")
               (let ((val (read)))
                 (update-field record (get-field-index field fields) val)
                 (display val)
                 (newline))
               #t))
            (else (begin (display field)
                         (display " is not valid")
                         (newline)
                         #f))))))

(define edit-record
  (lambda (index)
    (cond ((record-index-valid? index)
           (let ((record (list-ref (db-records (current-db)) (- index 1)))
                 (fields (db-fields (current-db))))
             (display-record record
                             fields
                             index)
             (if (ask-to-edit record fields)
                 (edit-record index)
                 (display "Edited"))))
          (else (display index)
                (display " is not a valid record")
                (newline)))))

(define save-db
  (lambda ()
    (delete-if-exists (db-filename (current-db)))
    (let ((port (open-output-file (db-filename (current-db)))))
      (write (db-filename (current-db)) port)
      (newline port)
      (write (db-fields (current-db)) port)
      (newline port)
      (write (db-records (current-db)) port)
      (newline port)
      (close-output-port port))))

(define load-db
  (lambda (filename)
    (clear-current-db!)
    (let ((port (open-input-file filename)))
      (let ((filename (read port))
            (fields (read port)))
        (new-db filename fields)
        (db-set-records! (current-db) (read port)))
      (close-input-port port))))

(define (clear-current-db!)
  (if (not (no-db?))
      (begin
        (if (ask "Do you want to save the current database?")
            (save-db)
            void)
        (vector-set! current-state 0 #f))
      void))

(define get
  (lambda (fieldname record)
    (let ((fields (current-fields)))
      (vector-ref record (get-field-index fieldname fields)))))

(define blank-record
  (lambda ()
    (make-vector (length (current-fields)) #f)))

;; NOTE: Just created an adapter to incorporate the suggested interface into
;; my first implementation.
(define record-set!
  (lambda (fieldname record value)
    (let ((fields (current-fields)))
      (update-field record (get-field-index fieldname fields) value))))

(define (sort-db predicate)
  ;; Access record list from db
  
  (db-set-records! (current-db)
                   (sort-mod predicate (current-records))))

(define sort-on-by
  (lambda (fieldname predicate)
    (sort-db (lambda (r1 r2) (predicate (get fieldname r1)
                                        (get fieldname r2))))))

(define generic-before?
  (lambda (arg1 arg2)
    (cond ((and (number? arg1) (number? arg2))
           (< arg1 arg2))
          ((and (word? arg1) (word? arg2))
           (before? arg1 arg2))
          ((word? arg1) (generic-before? (list arg1) arg2))
          ((word? arg2) (generic-before? arg1 (list arg2)))
          (else (sent-before? arg1 arg2)))))