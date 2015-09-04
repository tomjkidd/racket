#lang racket
(require (planet dyoo/simply-scheme:2))
(require racket/trace)

(define total-cols 26)
(define total-rows 40)
(define window-cols 6)
(define window-rows 20)

(define default-digits 2)

(define modified-counter 0)
(define (increment-modified-counter)
  (set! modified-counter (+ modified-counter 1)))
(define (clear-modified-counter)
  (set! modified-counter 0))

(define number-of-digits-vector (make-vector total-cols default-digits))
(define (number-of-digits-set! col value)
  (modified-column-width->history col
                                  (vector-ref number-of-digits-vector (- col 1)))
  (vector-set! number-of-digits-vector (- col 1) value))
(define (number-of-digits-ref col)
  (vector-ref number-of-digits-vector (- col 1)))

;; History
(define previous-command-name 'none)
(define (previous-command-name->history name)
  (set! previous-command-name name))
(define (history->previous-command-name)
  previous-command-name)

(define previous-selection-cell-id-index 0)
(define previous-corner-cell-id-index 1)

(define history-vector (make-vector 2 0))

(define (save-cell-to-history index value)
  (let ((entry (vector-ref history-vector index)))
    (if (equal? entry 0)
      (vector-set! history-vector index (make-id 1 1))
      (vector-set! history-vector index value))))

(define (selection-cell-id->history)
  (save-cell-to-history previous-selection-cell-id-index (selection-cell-id)))

(define (history->selection-cell-id)
  (vector-ref history-vector previous-selection-cell-id-index))

(define (corner-cell-id->history)
  (save-cell-to-history previous-corner-cell-id-index (screen-corner-cell-id)))

(define (history->corner-cell-id)
  (vector-ref history-vector previous-corner-cell-id-index))

(define previous-modified-cell-list '())

(define (modified-cell->history id formula)
  (set! previous-modified-cell-list
        (cons (list id formula) previous-modified-cell-list)))

(define previous-column-widths-vector (make-vector total-cols default-digits))

(define (modified-column-width->history col value)
  (vector-set! previous-column-widths-vector (- col 1) value))

(define (spreadsheet)
  (init-array)
  (set-selection-cell-id! (make-id 1 1))
  (set-screen-corner-cell-id! (make-id 1 1))
  (command-loop))

(define (command-loop)
  (print-screen)
  (clear-modified-counter)
  (let ((command-or-formula (read)))
    (if (equal? command-or-formula 'exit)
	"Bye!"
	(begin (process-command command-or-formula)
	       (command-loop)))))

(define (process-command command-or-formula)
  (cond ((and (list? command-or-formula)
              (command? (car command-or-formula)))
         (execute-command command-or-formula))
        ((command? command-or-formula)
         (execute-command (list command-or-formula 1)))
        (else (exhibit (ss-eval (pin-down command-or-formula
                                          (selection-cell-id)))))))

(define (execute-command command)
  (apply (get-command (car command))
	 (cdr command))
  (if (not (equal? (car command) 'undo))
      (previous-command-name->history (car command))
      void))

(define (exhibit val)
  (show val)
  (show "Type RETURN to redraw screen")
  (read-line)
  (read-line))


;;; Commands

;; Cell selection commands: F, B, N, P, and SELECT

(define (prev-row delta)
  (let ((row (id-row (selection-cell-id))))
    (if (< (- row delta) 1)
	(error "Already at top.")
	(set-selected-row! (- row delta)))))

(define (next-row delta)
  (let ((row (id-row (selection-cell-id))))
    (if (> (+ row delta) total-rows)
	(error "Already at bottom.")
	(set-selected-row! (+ row delta)))))

(define (prev-col delta)
  (let ((col (id-column (selection-cell-id))))
    (if (< (- col delta) 1)
	(error "Already at left.")
	(set-selected-column! (- col delta)))))

(define (next-col delta)
  (let ((col (id-column (selection-cell-id))))
    (if (> (+ col delta) total-cols)
	(error "Already at right.")
	(set-selected-column! (+ col delta)))))

(define (set-selected-row! new-row)
  (select-id! (make-id (id-column (selection-cell-id)) new-row)))

(define (set-selected-column! new-column)
  (select-id! (make-id new-column (id-row (selection-cell-id)))))

(define (select-id! id)
  (set-selection-cell-id! id)
  (adjust-screen-boundaries))

(define (select cell-name)
  (select-id! (cell-name->id cell-name)))

(define (adjust-screen-boundaries)
  (let ((row (id-row (selection-cell-id)))
        (col (id-column (selection-cell-id))))
    (if (< row (id-row (screen-corner-cell-id)))
        (set-corner-row! row)
        'do-nothing)
    (if (>= row (+ (id-row (screen-corner-cell-id)) window-rows))
        (set-corner-row! (- row (- window-rows 1)))
        'do-nothing)
    (if (< col (id-column (screen-corner-cell-id)))
        (set-corner-column! col)
        'do-nothing)
    (if (>= col (+ (id-column (screen-corner-cell-id)) window-cols))
        (set-corner-column! (- col (- window-cols 1)))
        'do-nothing)))

(define (set-corner-row! new-row)
  (set-screen-corner-cell-id!
   (make-id (id-column (screen-corner-cell-id)) new-row)))

(define (set-corner-column! new-column)
  (set-screen-corner-cell-id!
   (make-id new-column (id-row (screen-corner-cell-id)))))


;; LOAD

(define (spreadsheet-load filename)
  (let ((port (open-input-file filename)))
    (sl-helper port)
    (close-input-port port)))

(define (sl-helper port)
  (let ((command (read port)))
    (if (eof-object? command)
	'done
	(begin (show command)
	       (process-command command)
	       (sl-helper port)))))


;; PUT

(define (put formula . where)
  (cond ((null? where)
         (put-formula-in-cell formula (selection-cell-id)))
        ((cell-name? (car where))
         (put-formula-in-cell formula (cell-name->id (car where))))
        ((number? (car where))
         (put-all-cells-in-row formula (car where)))
        ((letter? (car where))
         (put-all-cells-in-col formula (letter->number (car where))))
        (else (error "Put it where?")))
  (display-modified-cell-count))

(define (put-all-cells-in-row formula row)
  (put-all-helper formula (lambda (col) (make-id col row)) 1 total-cols))

(define (put-all-cells-in-col formula col)
  (put-all-helper formula (lambda (row) (make-id col row)) 1 total-rows))

(define (put-all-helper formula id-maker this max)
  (if (> this max)
      'done
      (begin (try-putting formula (id-maker this))
	     (put-all-helper formula id-maker (+ 1 this) max))))

(define (try-putting formula id)
  (if (or (null? (cell-value id)) (null? formula))
      (put-formula-in-cell formula id)
      'do-nothing))

(define (put-formula-in-cell formula id)
  (increment-modified-counter)
  (modified-cell->history id (clone-cell id))
  (put-expr (pin-down formula id) id))

;; Window
(define (window . where)
  (cond ((cell-name? (car where))
         (set-screen-corner-cell-id!
          (let ((cell-id (cell-name->id (car where))))
            (ensure-valid-window-id (id-row cell-id)
                                    (id-column cell-id)))))
        ((and (number? (car where))
              (not (null? (cdr where)))
              (number? (cadr where)))
         (set-screen-corner-cell-id!
          (ensure-valid-window-id
           (+ (id-column (screen-corner-cell-id)) (cadr where))
           (+ (id-row (screen-corner-cell-id)) (car where)))))
        (else (error "Window error..."))))

(define (ensure-valid-window-id new-row new-col)
  (let ((row-and-col (make-vector 2)))
    (vector-set! row-and-col 0 new-row)
    (vector-set! row-and-col 1 new-col)
    
    (if (> (+ window-rows (vector-ref row-and-col 0)) total-rows)
        (vector-set! row-and-col 0 (- total-rows 19))
        'void)
    (if (< (vector-ref row-and-col 0) 1)
        (vector-set! row-and-col 0 1)
        'void)
    
    (if (> (+ window-cols (vector-ref row-and-col 1)) total-cols)
        (vector-set! row-and-col 1 (- total-cols 5))
        'void)
    (if (< (vector-ref row-and-col 1) 1)
        (vector-set! row-and-col 1 1)
        'void)
    
    (make-id (vector-ref row-and-col 1) (vector-ref row-and-col 0))))

;; Column Width
(define (column-width . args)
  (cond ((and (number? (car args)) (null? (cdr args)))
         (column-width-helper 1 (car args)))
        ((and (letter? (car args)) (number? (cadr args)))
         (update-column-width-history)
         (number-of-digits-set! (letter->number (car args)) (cadr args)))
        ((and (number? (car args)) (number? (cadr args)))
         (update-column-width-history)
         (number-of-digits-set! (car args) (cadr args)))
        (else (error "Column Width error"))))

;; Write to all cells with existing values (to capture for history)
(define (update-column-width-history)
  (vector-for-each (lambda (history index)
                     (number-of-digits-set! (+ index 1) history))
                   number-of-digits-vector))

(define (column-width-helper index value)
  (if (> index total-cols)
      'done
      (begin
        (number-of-digits-set! index value)
        (column-width-helper (+ index 1) value))))

;; Undo
(define (undo)
  (let ((prev (history->previous-command-name)))
    (cond ((member? prev '(n p b f select))
           (set-selection-cell-id! (history->selection-cell-id)))
          ((equal? 'put prev)
           (let ((modified-cells (list-copy previous-modified-cell-list)))
             #|
             Clear out the list now that we have a copy, so that as we iterate through
             the copy and undo, these new values will be captured and also undo-able.
             |#
             (set! previous-modified-cell-list '())
             (for-each (lambda (history)
                         ;;(display history)
                         (put-formula-in-cell (vector-ref (cadr history) 1) (car history)))
                       modified-cells)))
          ((equal? 'window prev)
           (set-screen-corner-cell-id! (history->corner-cell-id)))
          ((equal? 'column-width prev)
           (let ((modified-widths (vector-copy previous-column-widths-vector)))
             (vector-for-each (lambda (history index)
                                (number-of-digits-set! (+ index 1) history))
                              modified-widths)))
          (else 'do-nothing))))

;; Inspired by http://stackoverflow.com/questions/20802629/copy-of-a-list-or-something-else-in-scheme
(define (list-copy lst)
  (if (null? lst) '()
      (cons (car lst) (list-copy (cdr lst)))))

(define (vector-copy vec)
  (vector-copy-helper 0 vec (make-vector (vector-length vec))))

(define (vector-copy-helper index old new)
  (if (= index (vector-length old))
      new
      (begin
        (vector-set! new index (vector-ref old index))
        (vector-copy-helper (+ index 1) old new))))

(define (vector-for-each fn vec)
  (vector-for-each-helper 0 vec fn))

(define (vector-for-each-helper index vec fn)
  (if (< index (vector-length vec))
      (begin
        (fn (vector-ref vec index) index)
        (vector-for-each-helper (+ index 1) vec fn))
      void))

;;; The Association List of Commands

(define (command? name)
  (assoc name *the-commands*))

(define (get-command name)
  (let ((result (assoc name *the-commands*)))
    (if (not result)
        #f
        (cadr result))))

(define *the-commands*
  (list (list 'p prev-row)
        (list 'n next-row)
        (list 'b prev-col)
        (list 'f next-col)
        (list 'select select)
        (list 'put put)
        (list 'load spreadsheet-load)
        (list 'window window)
        (list 'column-width column-width)
        (list 'undo undo)))


;;; Pinning Down Formulas Into Expressions

(define (pin-down formula id)
  (cond ((cell-name? formula) (cell-name->id formula))
        ((word? formula) formula)
        ((null? formula) '())
        ((equal? (car formula) 'cell)
         (pin-down-cell (cdr formula) id))
        (else (bound-check
               (map (lambda (subformula) (pin-down subformula id))
                    formula)))))

(define (bound-check form)
  (if (member 'out-of-bounds form)
      'out-of-bounds
      form))

(define (pin-down-cell args reference-id)
  (cond ((null? args)
	 (error "Bad cell specification: (cell)"))
	((null? (cdr args))
	 (cond ((number? (car args))         ; they chose a row
		(make-id (id-column reference-id) (car args)))
	       ((letter? (car args))         ; they chose a column
		(make-id (letter->number (car args))
			 (id-row reference-id)))
	       (else (error "Bad cell specification:"
			    (cons 'cell args)))))
	(else
	 (let ((col (pin-down-col (car args) (id-column reference-id)))
	       (row (pin-down-row (cadr args) (id-row reference-id))))
	   (if (and (>= col 1) (<= col total-cols) (>= row 1) (<= row total-rows))
	       (make-id col row)
	       'out-of-bounds)))))

(define (pin-down-col new old)
  (cond ((equal? new '*) old)
	((equal? (first new) '>) (+ old (bf new)))
	((equal? (first new) '<) (- old (bf new)))
	((letter? new) (letter->number new))
	(else (error "What column?"))))

(define (pin-down-row new old)
  (cond ((number? new) new)
	((equal? new '*) old)
	((equal? (first new) '>) (+ old (bf new)))
	((equal? (first new) '<) (- old (bf new)))
	(else (error "What row?"))))


;;; Dependency Management

(define (put-expr expr-or-out-of-bounds id)
  (let ((expr (if (equal? expr-or-out-of-bounds 'out-of-bounds)
		  '()
		  expr-or-out-of-bounds)))
    (for-each (lambda (old-parent)
		(set-cell-children!
		 old-parent
		 (rmv id (cell-children old-parent))))
	      (cell-parents id))
    (set-cell-expr! id expr)
    (set-cell-parents! id (remdup (extract-ids expr)))
    (for-each (lambda (new-parent)
		(set-cell-children!
		 new-parent
		 (cons id (cell-children new-parent))))
	      (cell-parents id))
    (figure id)))

(define (extract-ids expr)
  (cond ((id? expr) (list expr))
	((word? expr) '())
	((null? expr) '())
	(else (append (extract-ids (car expr))
		      (extract-ids (cdr expr))))))

(define (figure id)
  (cond ((null? (cell-expr id)) (setvalue id '()))
	((all-evaluated? (cell-parents id))
	 (setvalue id (ss-eval (cell-expr id))))
	(else (setvalue id '()))))

(define (all-evaluated? ids)
  (cond ((null? ids) #t)
	((not (number? (cell-value (car ids)))) #f)
	(else (all-evaluated? (cdr ids)))))

(define (setvalue id value)
  (let ((old (cell-value id)))
    (set-cell-value! id value)
    (if (not (equal? old value))
	(for-each figure (cell-children id))
	'do-nothing)))


;;; Evaluating Expressions

(define (ss-eval expr)
  (cond ((number? expr) expr)
	((quoted? expr) (quoted-value expr))
	((id? expr) (cell-value expr))
	((invocation? expr)
	 (apply (get-function (car expr))
		(map ss-eval (cdr expr))))
	(else (error "Invalid expression:" expr))))

(define (quoted? expr)
  (or (string? expr)
      (and (list? expr) (equal? (car expr) 'quote))))

(define (quoted-value expr)
  (if (string? expr)
      expr
      (cadr expr)))

(define (invocation? expr)
  (list? expr))

(define (get-function name)
  (let ((result (assoc name *the-functions*)))
    (if (not result)
	(error "No such function: " name)
	(cadr result))))

(define *the-functions*
  (list (list '* *)
	(list '+ +)
	(list '- -)
	(list '/ /)
	(list 'abs abs)
	(list 'acos acos)
	(list 'asin asin)
	(list 'atan atan)
	(list 'ceiling ceiling)
	(list 'cos cos)
	(list 'count count)
	(list 'exp exp)
	(list 'expt expt)
	(list 'floor floor)
	(list 'gcd gcd)
	(list 'lcm lcm)
	(list 'log log)
	(list 'max max)
	(list 'min min)
	(list 'modulo modulo)
	(list 'quotient quotient)
	(list 'remainder remainder)
	(list 'round round)
	(list 'sin sin)
	(list 'sqrt sqrt)
	(list 'tan tan)
	(list 'truncate truncate)))

;;; Printing the Screen

(define (print-screen)
  (newline)
  (newline)
  (newline)
  (show-column-labels (id-column (screen-corner-cell-id)))
  (show-rows window-rows
	     (id-column (screen-corner-cell-id))
	     (id-row (screen-corner-cell-id)))
  (display-cell-name (selection-cell-id))
  (display ":  ")
  (show (cell-value (selection-cell-id)))
  (display-expression (cell-expr (selection-cell-id)))
  (newline)
  ;;(display previous-column-widths-vector)
  (display "?? "))

(define (display-modified-cell-count)
  (display modified-counter)
  (display " cells modified")
  (newline))

(define (display-cell-name id)
  (display (number->letter (id-column id)))
  (display (id-row id)))

(define (show-column-labels col-number)
  (display "  ")
  (show-label window-cols col-number)
  (newline))

(define (show-label to-go this-col-number)
  (cond ((= to-go 0) '())
	(else
	 (display "  -----")
	 (display (number->letter this-col-number))
	 (display "----")
	 (show-label (- to-go 1) (+ 1 this-col-number)))))

(define (show-rows to-go col row)
  (cond ((= to-go 0) 'done)
	(else
	 (display (align row 2 0))
	 (display " ")
	 (show-row window-cols col row)
	 (newline)
	 (show-rows (- to-go 1) col (+ row 1)))))

(define (show-row to-go col row)
  (cond ((= to-go 0) 'done)
	(else
	   (display (if (selected-indices? col row) ">" " "))
	   (display-value (cell-value-from-indices col row) (number-of-digits-ref col))
	   (display (if (selected-indices? col row) "<" " "))
	   (show-row (- to-go 1) (+ 1 col) row))))

(define (selected-indices? col row)
  (and (= col (id-column (selection-cell-id)))
       (= row (id-row (selection-cell-id)))))

(define (display-value val num-digits)
  (display (align (if (null? val) "" val) 10 num-digits)))

(define (display-expression expr)
  (cond ((null? expr) (display '()))
        ((quoted? expr) (display (quoted-value expr)))
        ((word? expr) (display expr))
        ((id? expr)
         (display-cell-name expr))
        (else (display-invocation expr))))

(define (display-invocation expr)
  (display "(")
  (display-expression (car expr))
  (for-each (lambda (subexpr)
	      (display " ")
	      (display-expression subexpr))
	    (cdr expr))
  (display ")"))
      

;;; Abstract Data Types

;; Special cells: the selected cell and the screen corner

(define *special-cells* (make-vector 2))

(define (selection-cell-id)
  (vector-ref *special-cells* 0))

(define (set-selection-cell-id! new-id)
  (selection-cell-id->history)
  (vector-set! *special-cells* 0 new-id))

(define (screen-corner-cell-id)
  (vector-ref *special-cells* 1))

(define (set-screen-corner-cell-id! new-id)
  (corner-cell-id->history)
  (vector-set! *special-cells* 1 new-id))


;; Cell names

(define (cell-name? expr)
  (and (word? expr)
       (letter? (first expr))
       (number? (bf expr))))

(define (cell-name-column cell-name)
  (letter->number (first cell-name)))

(define (cell-name-row cell-name)
  (bf cell-name))

(define (cell-name->id cell-name)
  (make-id (cell-name-column cell-name)
	   (cell-name-row cell-name)))
	
;; Cell IDs

(define (make-id col row)
  (list 'id col row))

(define (id-column id)
  (cadr id))

(define (id-row id)
  (caddr id))

(define (id? x)
  (and (list? x)
       (not (null? x))
       (equal? 'id (car x))))

;; Cells

(define (make-cell)
  (vector '() '() '() '()))

(define (cell-value id)
  (vector-ref (cell-structure id) 0))

(define (cell-value-from-indices col row)
  (vector-ref (cell-structure-from-indices col row) 0))

(define (cell-expr id)
  (vector-ref (cell-structure id) 1))

(define (cell-parents id)
  (vector-ref (cell-structure id) 2))

(define (cell-children id)
  (vector-ref (cell-structure id) 3))

(define (set-cell-value! id val)
  (vector-set! (cell-structure id) 0 val))

(define (set-cell-expr! id val)
  (vector-set! (cell-structure id) 1 val))

(define (set-cell-parents! id val)
  (vector-set! (cell-structure id) 2 val))

(define (set-cell-children! id val)
  (vector-set! (cell-structure id) 3 val))

(define (cell-structure id)
  (global-array-lookup (id-column id)
		       (id-row id)))

(define (cell-structure-from-indices col row)
  (global-array-lookup col row))

(define (clone-cell id)
  (let ((cell (make-cell)))
    (vector-set! cell 0 (cell-value id))
    (vector-set! cell 1 (cell-expr id))
    (vector-set! cell 2 (cell-parents id))
    (vector-set! cell 3 (cell-children id))
    cell))

(define *the-spreadsheet-array* (make-vector total-rows))

(define (global-array-lookup col row)
  (if (and (<= row total-rows) (<= col total-cols))
      (vector-ref (vector-ref *the-spreadsheet-array* (- row 1))
		  (- col 1))
      (error "Out of bounds")))

(define (init-array)
  (fill-array-with-rows (- total-rows 1)))

(define (fill-array-with-rows n)
  (if (< n 0)
      'done
      (begin (vector-set! *the-spreadsheet-array* n (make-vector total-cols))
	     (fill-row-with-cells
	      (vector-ref *the-spreadsheet-array* n) (- total-cols 1))
	     (fill-array-with-rows (- n 1)))))

(define (fill-row-with-cells vec n)
  (if (< n 0)
      'done
      (begin (vector-set! vec n (make-cell))
	     (fill-row-with-cells vec (- n 1)))))

;;; Utility Functions

(define alphabet
  '#(a b c d e f g h i j k l m n o p q r s t u v w x y z))

(define (letter? something)
  (and (word? something)
       (= 1 (count something))
       (vector-member something alphabet)))

(define (number->letter num)
  (vector-ref alphabet (- num 1)))

(define (letter->number letter)
  (+ (vector-member letter alphabet) 1))

(define (vector-member thing vector)
  (vector-member-helper thing vector 0))

(define (vector-member-helper thing vector index)
  (cond ((= index (vector-length vector)) #f)
	((equal? thing (vector-ref vector index)) index)
	(else (vector-member-helper thing vector (+ 1 index)))))

(define (remdup lst)
  (cond ((null? lst) '())
	((member (car lst) (cdr lst))
	 (remdup (cdr lst)))
	(else (cons (car lst) (remdup (cdr lst))))))

(define (rmv bad-item lst)
  (filter (lambda (item) (not (equal? item bad-item)))
	  lst))

#! 25.1
#|
The only trick was to do find-replace:
26 -> total-cols
30 -> total-rows
25 -> (- total-cols 1)
29 -> (- total-rows 1)
|#
;;(spreadsheet)

#! 25.2
#|
In a style similar to binary or hex, you could use the alphabet as a counting system
binary
------
0
1
10
11
100
101
110
111
1000

using the alphabet
------------------
map each letter to a number. Note that a program like excel goes to aa after z.
This seems reasonable enough for a system to model

a -> 1
b -> 2
c -> 3
d -> 4
...
x -> 24
y -> 25
z -> 26
aa -> 27
ab -> 28 ...

Which functions need to change to support this?
cell-name? assumes single character for letter/number. This would need to change.
cell-name-column would also makes a similar letter assumption.
cell-name-row assumes butfirst on name is all row number. This would need to change, too
|#

(define (col-name->num col-name)
  (col-name-helper 0 (string->list (string-upcase col-name))))

(define (char-diff char1 char2)
  (let ((int-1 (char->integer char1))
        (int-2 (char->integer char2)))
    (- int-1 int-2)))

(define (char-diff-from-A char)
  (char-diff char #\A))

(define (col-name-helper num remaining)
  (if (null? remaining)
      num
      (let ((next-num (+ (* num 26) (char-diff-from-A (car remaining)) 1)))
        (col-name-helper next-num (cdr remaining)))))
    
(col-name->num "a")
(col-name->num "z")
(col-name->num "aa")
(col-name->num "ah")
(col-name->num "xfd")

#! 25.3
#| Copied definitions for reference:

(define *the-spreadsheet-array* (make-vector total-rows))

(define (global-array-lookup col row)
  (if (and (<= row total-rows) (<= col total-cols))
      (vector-ref (vector-ref *the-spreadsheet-array* (- row 1))
		  (- col 1))
      (error "Out of bounds")))

(define (init-array)
  (fill-array-with-rows (- total-rows 1)))

(define (fill-array-with-rows n)
  (if (< n 0)
      'done
      (begin (vector-set! *the-spreadsheet-array* n (make-vector total-cols))
	     (fill-row-with-cells
	      (vector-ref *the-spreadsheet-array* n) (- total-cols 1))
	     (fill-array-with-rows (- n 1)))))

(define (fill-row-with-cells vec n)
  (if (< n 0)
      'done
      (begin (vector-set! vec n (make-cell))
	     (fill-row-with-cells vec (- n 1)))))

In order get things working for a single array, the array of arrays must be
mapped in some way to a single array.

(col, row) -> index of array

The most natural way I know to do this is to enumerate by col, then row.
[
  [1 2 3]
  [4 5 6]
  [7 8 9]
]
Note: For now col and row are 0 indexed!
So, (get-index col row) -> (+ (* number-of-colums row) col)
(get-index 1 0) -> (+ (* 3 0) 1) = 1
(get-index 1 1) -> (+ (* 3 1) 1) = 4

TODO: Change init-array and global-array-lookup to use new mapping.

|#

#! 25.4
#|
Copied the definitions for reference:

(define (get-command name)
  (let ((result (assoc name *the-commands*)))
    (if (not result)
        #f
        (cadr result))))

(define (get-function name)
  (let ((result (assoc name *the-functions*)))
    (if (not result)
	(error "No such function: " name)
	(cadr result))))

get-command returns false, while get-function produces an error.

process-command uses a cond to determine if what the user provides is valid.
If valid, execute-command is called, which uses get-command.
If not valid, get-command isn't called.

ss-eval is the only place where get-function is used.
ss-eval is attempted if there is no command match in process-command.
ss-eval will encounter the error produced by get-function when trying to handle
an invalid user command.
|#

#! 25.6
#|
When a command is issued like:
(put 'test)

In the put call, the first condition matches, so put-formula-in-cell is called.
put-formula-in-cell will pass it's result to pin-down.

How is the formula translated into an expression?
The formula is a word.
pin-down evaluates the word? condition, which just provides the word as the expression.

How is that expression evaluated?
This word then goes to put-expr, which uses the word as it's value.

What if the labeled cell has children?

TODO: If you type exit into the console, the state of the table remains,
you can then use commands like (global-array-lookup 1 1) to investigate cells.
Create a case to investigate this scenario.

Enter the following commands after (spreadsheet)
(put (+ a1 0) a2)
(put 1 a1)
(put 'test a1)
exit

> (global-array-lookup 2 1)
'#(() () () ())
> (global-array-lookup 1 2)
'#(() (+ (id 1 1) 0) ((id 1 1)) ())
> (global-array-lookup 1 1)
'#(test 'test () ((id 1 2)))

Note that the children are the third element of the cell, and even though the
text has been put in, the dependency still is there.
In this case the formula in the cell needs a number, so it is blank in the sheet.

|#
;;(trace get-command)
;;(trace put)
;; NOTE: put is not traced due to apply being used to call it! Didn't expect that.
;;(trace put-formula-in-cell)
;;(trace pin-down) ;; -> ''test
;;(trace put-expr)
;;(trace set-cell-expr!)
;;(trace cell-name?)
;;(trace put-all-cells-in-col)
;;(trace put-all-helper)
;;(trace process-command)
;;(trace global-array-lookup)
#|

>(get-command 'put)
<#<procedure:put>
>(put-formula-in-cell ''test '(id 1 1))
> (pin-down ''test '(id 1 1))
> >(pin-down 'quote '(id 1 1))
< <'quote
> >(pin-down 'test '(id 1 1))
< <'test
< ''test
>(put-expr ''test '(id 1 1))
> (set-cell-expr! '(id 1 1) ''test)
< #<void>
<#<void>

|#

#|
Valid commands
(put 1 a) ;; -> put 1 in all cells in col a
(put () a) ;; -> erase value from all cells in col a
(put 2 1) ;; -> put 2 in all cells in row 1
(put () 1) ;; -> erase value from all cells in row 1
(select a1) ;; -> select cell a1

(put (* (cell a) (cell b)) c) ;; -> store the product of col a and col b in col c for each row
(put 2 a1) ;; -> put 2 in cell a1
(put 6 b1) ;; -> put 6 in cell b1
(select c1) ;; -> select cell c1 (to see expression and value)

(put () b3)
(put () c5)
(put () d6)
(put (+ b3 c5) d6)
(put 2 b3)
(put 3 c5)
(select d6)
(put 4 c5)
(put () b3)

|#

#! 25.7
#|
More notes in exercise7-notes.md...

Created the window function
|#
;;(trace ensure-valid-window-id)

#! 25.8
#|
In order to accomodate keeping track of the number of modified cells:
1. Create a variable as a 1 element vector.
2. Find places where updates can occur and increment the variable at those points.
       put-formula-in-cell is where this occurs.
3. Clear the variable at the start of each command
4. Display the variable at the end of put command

NOTE: I looked at racket's documentation and saw that using a variable is more
straight forward than creating a vector to store the value, so I decided to use
a variable instead. You could do it just the same with a 1 element vector in order
to stay aligned with only functions introduced by Simply Scheme.

The variable named modified-counter was created to keep track of the count for the
current command.
The increment-modified-counter function was created to allow easy increment.
The clear-modified-counter function was created to allow easy clear.
|#

#! 25.9
#|
In order to allow each column to track number of decimal places...
1. Create a vector that contains a spot for each column
2. Create a command to set this value for a column
    (column-width [val]) -> set for all columns
    (column-width [col] [val]) -> set for a specific column by index (1-based)
    (column-width [letter] [val]) -> set for a specific column by letter
3. Modify print-screen to use this information, display-value function is where.

TODO: Create a file of commands to demonstrate functionality works
(column-width 5) ;; All columns to 5 deciaml places
(column-width a 3) ;; Column a to 3 decimal places
(column-width 2 2) ;; Column b to 2 decimal places

(put a1 1.12345)
(put b1 1.12345)
(put c1 1.12345)

|#

#! 25.10
#|
load and exit don't need to work for undo
undo of cell select should return to previous cell
undo of put will re-put the previous expressions in all affected cells

In order to provide undo:
1. For each command that changes the selected cell, save previous selected cell.
    Modify set-selection-cell-id! to capture a change

2. For each command that changes one or more cell values, save a list of the
cell-ids as well as the cells.
    Modify put-formula-in-cell to save the cell id and previous cell before writing a new one.
    Create a clone-cell function to clone the values in a cell
    Create a list-copy function to clone the list of modified cells so that undo will work with undo

3. For the window command, keep track of the previous corner cell
    Modify set-screen-corner-cell-id! to capture values

4. For the column-width command, keep track with a snapshot list of the last state
    Modify number-of-digits-set! to capture values

5. When undo is called, restore each cell based on the list, then restore previous
selection.

6. Clear history (previous selected cell, previous corner cell, column width list,
and modified cell list) when a new command is entered (after read).

7. Keep track of execute-command to tell which command ran last

TODO: Once all work, keep track of the name of the command that ran last,
and use that to determine which piece of history to restore. If undo, just keep as is.

(put 0.123456 1)
(column-width 5)
(column-width b 3)
(undo)
(column-width b 4)
(column-width 2)
(undo)

|#

#! 25.11
#|
Add an accumulate procedure that can be used as a formula

Rectangular mapping
c2 c7 -> c2 c3 c4 c5 c6 c7
a3 c5 -> a3 b3 c3 a4 b4 c4 a5 b5 c5

1. Create a function that takes the two cell names and returns a list of cells
2. Add accumulate to *the-functions*
3. Reshape the call, where (first args)-> operation and (cdr args) is the list of cells
4. Modify pin-down to convert accumulate into the spelled-out form
|#
(define (col-and-row->cell-name col row)
  (word (number->letter col)
        row))

;; TODO: Implement this another way, counting down so the list builds in the right order
;; Also try to simplify the conditions.
(define (get-cell-name-range cell-name-a cell-name-b)
  (let ((current-row (cell-name-row cell-name-a))
        (current-col (cell-name-column cell-name-a))
        (end-row (cell-name-row cell-name-b))
        (end-col (cell-name-column cell-name-b)))
    (get-cell-name-range-helper '() current-col current-row current-col current-row end-col end-row)))

(define (get-cell-name-range-helper-legacy lst current-col current-row
                                    start-col start-row
                                    end-col end-row)
  (cond ((and (< current-col end-col) (<= current-row end-row))
         (let ((new-lst (cons (col-and-row->cell-name current-col current-row) lst)))
           (get-cell-name-range-helper new-lst (+ current-col 1) current-row
                                     start-col start-row
                                     end-col end-row)))
        ((and (= current-col end-col) (<= current-row end-row))
         (let ((new-lst (cons (col-and-row->cell-name current-col current-row) lst)))
           (get-cell-name-range-helper new-lst start-col (+ current-row 1)
                                       start-col start-row
                                       end-col end-row)))
        ((and (= current-col start-col) (= current-row (+ end-row 1)))
         (reverse lst))
        (else (error "Unexpected get-cell-name-range case"))))


;; This is my 'simpler' implementation, constructing each row as a list, then appending the rows together
;; TODO: Remove current-col and current-row, they aren't needed.
(define (get-cell-name-range-helper lst current-col current-row
                                    start-col start-row
                                    end-col end-row)
  (let ((col-range (range start-col (+ end-col 1) 1))
        (row-range (range start-row (+ end-row 1) 1)))
    (accumulate append (map (lambda (row)
                (map (lambda (col)
                            (col-and-row->cell-name col row))
                          col-range))
                            row-range))))

(col-and-row->cell-name 1 1)

(get-cell-name-range 'c2 'c7)
(get-cell-name-range 'a3 'c5)

;;(spreadsheet)