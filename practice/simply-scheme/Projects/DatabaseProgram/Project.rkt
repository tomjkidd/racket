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