#lang racket
(require (planet dyoo/simply-scheme:2))
(require racket/trace)

(define (delete-if-exists name)
  (if (file-exists? name)
      (delete-file name)
      void))

(delete-if-exists "songs")
(delete-if-exists "songs2")

(let ((port (open-output-file "songs")))
  (show '(all my loving) port)
  (show '(ticket to ride) port)
  (show '(martha my dear) port)
  (close-output-port port))

(define in (open-input-file "songs"))

(read in)
(read in)
(read in)

(close-input-port in)

(let ((port (open-output-file "songs2")))
  (show-line '(all my loving) port)
  (show-line '(ticket to ride) port)
  (show-line '(martha my dear) port)
  (close-output-port port))

(define in2 (open-input-file "songs2"))
(read-line in2)
(close-input-port in2)

(define (get-song n)
  (let ((port (open-input-file "songs2")))
    (skip-songs (- n 1) port)
    (let ((answer (read-line port)))
      (close-input-port port)
      answer)))

(define (skip-songs n port)
  (if (= n 0)
      'done
      (begin (read-line port)
             (skip-songs (- n 1) port))))

(get-song 2)

(define (print-file name)
  (let ((port (open-input-file name)))
    (print-file-helper port)
    (close-input-port port)
    'done))

(define (print-file-helper port)
  (let ((stuff (read-string port)))
    (if (eof-object? stuff)
        'done
        (begin (show-line stuff)
               (print-file-helper port)))))

(print-file "songs")

(define (file-map fn inname outname)
  (let ((inport (open-input-file inname))
        (outport (open-output-file outname)))
    (file-map-helper fn inport outport)
    (close-input-port inport)
    (close-output-port outport)
    'done))

(define (file-map-helper fn inport outport)
  (let ((line (read-line inport)))
    (if (eof-object? line)
        'done
        (begin (show-line (fn line) outport)
               (file-map-helper fn inport outport)))))

(define (lastfirst name)
  (se (word (last name) ",") (bl name)))

(delete-if-exists "dddbmt")
(delete-if-exists "dddbmt-reversed")

(let ((port (open-output-file "dddbmt")))
  (show "David Harmon" port)
  (show "Trevor Davies" port)
  (show "John Dymond" port)
  (show "Michael Wilson" port)
  (show "Ian Amey" port)
  (close-output-port port))


(file-map lastfirst "dddbmt" "dddbmt-reversed")
(print-file "dddbmt-reversed")

(delete-if-exists "grades")
(delete-if-exists "results")

(let ((port (open-output-file "grades")))
  (show "John 88 92 100 75 95" port)
  (show "Paul 90 91 85 80 91" port)
  (show "George 85 87 90 72 96" port)
  (show "Ringo 95 84 88 87 87" port)
  (close-output-port port))

(define (process-grades line)
  (se (first line)
      "total:"
      (accumulate + (bf line))
      "average:"
      (/ (accumulate + (bf line)) (count (bf line)))))

(file-map process-grades "grades" "results")
(print-file "results")

(delete-if-exists "r5rs-just")

(define (justify line width)
  (if (< (count line) 2)
      line
      (se (pad line
               (- (count line) 1)
               (extra-spaces width (char-count line))))))

(define (char-count line)
  (+ (accumulate + (every count line))
     (- (count line) 1)))

(define (extra-spaces width chars)
  (if (> chars width)
      0
      (- width chars)))

(define (pad line chances needed)
  (if (= chances 0)
      (first line)
      (let ((extra (quotient needed chances)))
        (word (first line)
              (spaces (+ extra 1))
              (pad (bf line) (- chances 1) (- needed extra))))))

(define (spaces n)
  (if (= n 0)
      ""
      (word " " (spaces (- n 1)))))

(file-map (lambda (sent) (justify sent 50)) "r5rs" "r5rs-just")
(print-file "r5rs-just")