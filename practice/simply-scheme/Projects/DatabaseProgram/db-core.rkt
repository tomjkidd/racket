#lang racket

(require (planet dyoo/simply-scheme:2))
(require racket/trace)

(provide make-db
         db-filename
         db-set-filename!
         db-fields
         db-set-fields!
         db-records
         db-set-records!)

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
