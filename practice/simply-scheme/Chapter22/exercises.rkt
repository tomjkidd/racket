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
        (begin (show stuff)
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

(define (filemerge file1 file2 outfile)
  (let ((p1 (open-input-file file1))
        (p2 (open-input-file file2))
        (outp (open-output-file outfile)))
    (filemerge-helper p1 p2 outp (read-string p1) (read-string p2))
    (close-output-port outp)
    (close-input-port p1)
    (close-input-port p2)
    'done))

(define (filemerge-helper p1 p2 outp line1 line2)
  (cond ((eof-object? line1) (merge-copy line2 p2 outp))
        ((eof-object? line2) (merge-copy line1 p1 outp))
        ((before? line1 line2)
         (show line1 outp)
         (filemerge-helper p1 p2 outp (read-string p1) line2))
        (else (show line2 outp)
              (filemerge-helper p1 p2 outp line1 (read-string p2)))))

(define (merge-copy line inp outp)
  (if (eof-object? line)
      #f
      (begin (show line outp)
             (merge-copy (read-string inp) inp outp))))

(delete-if-exists "filemerge-combo")
(filemerge "filemerge1" "filemerge2" "filemerge-combo")

(print-file "filemerge-combo")
         
#! 22.1
(define (concatenate input-file-list output-file)
  (let ((outp (open-output-file output-file)))
    (concatenate-helper input-file-list outp)
    (close-output-port outp)))

(define (concatenate-helper input-file-list outp)
  (cond ((empty? input-file-list) 'done)
        (else (let ((inp (open-input-file (car input-file-list))))
                (begin
                  (file-map-helper (lambda (x) x) inp outp)
                  (close-input-port inp)
                  (concatenate-helper (cdr input-file-list) outp))))))

(delete-if-exists "concat-comb")
(concatenate (list "concat1" "concat2") "concat-comb")
(print-file "concat-comb")

#! 22.2
(define (file-map-no-output-port fn inname read-fn)
  (let ((inp (open-input-file inname)))
    (let ((result (file-map-helper-no-output-port fn inp read-fn)))
      (close-input-port inp)
      result)))

(define (file-map-helper-no-output-port fn inp read-fn)
  (let ((line (read-fn inp)))
    (if (eof-object? line)
        '()
        (cons (fn line)
              (file-map-helper-no-output-port fn inp read-fn)))))

(define (file-count-lines name)
  (accumulate + (file-map-no-output-port (lambda (line) 1) name read-line)))

(file-count-lines "countlinestest")

#! 22.3
(define (file-count-words name)
  (accumulate + (file-map-no-output-port words-in-line name read-line)))

(define (words-in-line line)
  (count line))

(trace words-in-line)
(file-count-words "countwordstest")

#! 22.4
(define (file-count-characters name)
  (accumulate + (file-map-no-output-port chars-in-line name read-string)))

(define (chars-in-line line)
  (count line)) ;; NOTE: This works because read-string is used as the read function!

(file-count-characters "countcharstest")

#! 22.5
(define (remove-immediate-dup-lines in-file-name out-file-name)
  (remove-dup-open-close-helper (lambda (line) line) in-file-name out-file-name ""))
      
(define (remove-dup-open-close-helper fn inname outname prev-line)
  (let ((inport (open-input-file inname))
        (outport (open-output-file outname)))
    (file-map-helper-rempove-dup fn prev-line inport outport)
    (close-input-port inport)
    (close-output-port outport)
    'done))

(define (file-map-helper-rempove-dup fn prev-line inport outport)
  (let ((line (read-line inport)))
    (if (eof-object? line)
        'done
        (begin (if (equal? line prev-line)
                   #f
                   (show-line (fn line) outport))
               (file-map-helper-rempove-dup fn line inport outport)))))

(delete-if-exists "dupline-result")
(remove-immediate-dup-lines "dupline" "dupline-result")
(print-file "dupline-result")

#! 22.6
"foghorn"
"horn"

(define (string-contains? str wd)
  (cond ((or (empty? str) (empty? wd)) #f)
        ((equal? (first str) (first wd)) (if (string-contains-helper (bf str) (bf wd))
                                             #t
                                             (string-contains? (bf str) wd)))
        (else (string-contains? (bf str) wd))))

(define (string-contains-helper str wd)
  (cond ((empty? wd) #t)
        ((empty? str) #f)
        ((equal? (first str) (first wd)) (string-contains-helper (bf str) (bf wd)))
        (else #f)))

;;(trace string-contains?)
;;(trace string-contains-helper)

(string-contains? "abcde" "e")
(string-contains? "abcde" "f")
(string-contains? "the fog is heavy" "fog")
(string-contains? "barfy" "")
(string-contains? "te" "tea")
(string-contains? "This is a test of the lookup function" "the")

(define (lookup filename wd)
  (let ((results 
         (filter (lambda (line) line)
                 (file-map-no-output-port (lambda (line) (if (string-contains? line wd)
                                                             line
                                                             #f))
                                          filename
                                          read-string))))
    (for-each (lambda (line) (show line)) results)))

(lookup "lookuptest" "the")
(lookup "lookuptest" "contain")
;; TODO: This can be better if you can specify whether or not to pay attention to case.

#! 22.7
(define (page-stream filename)
  (file-map-no-output-port-2 page-helper filename read-string))

(define (file-map-no-output-port-2 helper-fn inname read-fn)
  (let ((inp (open-input-file inname)))
    (helper-fn inp read-fn)
    (close-input-port inp)))

(define page-size 24)
(define history-size 1)

(define (page-helper inp read-fn)
  (show-pages inp read-fn '() page-size history-size))

(define (show-pages inp read-fn accum pg-size hist-size)
  (let ((result (consume-page inp read-fn accum pg-size #f)))
    (let ((page (car result))
          (eof-reached? (cadr result)))
      (show-page page)
      (show "end of page")
      (if eof-reached?
          (show "end of file")
          (begin
            ;; NOTE: Uncomment for user input to page
            ;;(read-line)
            (show-pages inp read-fn (reverse (take-last hist-size page)) pg-size hist-size))))))

(define (show-page page)
  (for-each (lambda (line) (show line)) page))

(define (consume-page inp read-fn accum pg-size eof-reached?)
  ;; TODO: Find a way to do this without reverse.
  (let ((line (read-fn inp)))
    (cond ((eof-object? line)
           (list (reverse accum) (eof-object? line)))
          ((= (length accum) (- pg-size 1))
           (list (reverse (cons line accum)) (eof-object? line)))
          (else
           (consume-page inp read-fn (cons line accum) pg-size (eof-object? line))))))

(define (take num lst)
  (if (= num 0)
      '()
      (cons (car lst) (take (- num 1) (cdr lst)))))

(define (drop num lst)
  (if (= num 0)
      lst
      (drop (- num 1) (cdr lst))))

(define (take-last num lst)
  (drop (- (length lst) num) lst))

;;(trace consume-page)
(page-stream "pagetest")

#! 22.8
(define (join file1-name file2-name file1-join-index file2-join-index out-filename)
  (let ((file1p (open-input-file file1-name))
        (file2p (open-input-file file2-name))
        (outp (open-output-file out-filename)))
    (join-helper file1p file2p outp file1-join-index file2-join-index #f #f)
    (close-input-port file1p)
    (close-input-port file2p)
    (close-output-port outp)))

(define (join-helper file1p file2p outp file1-join-index file2-join-index ln1 ln2)
  (let ((line1 (ensure-line file1p ln1))
        (line2 (ensure-line file2p ln2)))
    (if (or (eof-object? line1) (eof-object? line2))
        (show 'done)
        (let ((join-key-1 (list-ref line1 (- file1-join-index 1)))
              (join-key-2 (list-ref line2 (- file2-join-index 1))))
          (begin
            (cond ((equal? join-key-1 join-key-2)
                   (join-lines line1 line2 outp)
                   (join-helper file1p file2p outp file1-join-index file2-join-index #f #f))
                  ((before? join-key-1 join-key-2)
                   (join-helper file1p file2p outp file1-join-index file2-join-index #f line2))
                  ((before? join-key-2 join-key-1)
                   (join-helper file1p file2p outp file1-join-index file2-join-index line1 #f))))))))

(define (ensure-line file-port false-or-line)
  (if (equal? false-or-line #f)
      (read file-port)
      false-or-line))

(define (join-lines line1 line2 outp)
  (write (append (list (car line1)
                      (cadr line1)
                      (caddr line1))
                (cdr line2))
         outp)
  (newline outp))

(delete-if-exists "combined-file")
;;(trace join-helper)
(join "class-roster" "class-grades" 3 1 "combined-file")