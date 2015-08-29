Terms
-----
(term: definition)

Notes
-----
* Bullet list note

Program Components
------------------
(command processor: reads user commands, oversees their execution)
(commands: load and put)
(formula translator: turns a formula into an expression)
(dependency manager: cell expression dependency tracker)
(expression evaluator)
(screen printer)
(cell management: store/retrieve cell information)

Analysis
========
(spreadsheet) -> creates spreadsheet as vector of vectors (of cells),
  sets selection cell
  sets screen corner
  initializes command loop

(command-loop) -> prints screen, reads a line as a command or formula
  exits if the command is 'exit
  calls process-command on the command or formula
  calls command loop again

(process-command command-or-formula) -> execute a command, if a command
(execute-command command) -> calls apply with (get-command) and args
(get-command name) -> looks up command in *the-commands*
(pin-down formula id) ->

Types
-----
Id -> (list 'id col row)
Cell -> (vector '() '() '() '()), 0: value,
(expression: a formula whose cell references have been replaced by specific cell IDs. Expressions are put into cells.
  Expressions are 1 of 3 things:
  1. A constant expression
  2. A variable, cell ID
  3. A procedure invocation enclosed in parentheses.
  The evaluator is functional, doesn't modify state of the spreadsheet)
(pin down: the process of translating a formula to an expression)
(evaluator: takes an expression and computes a value)

Commands
--------
(prev-row delta) -> sets the selected row to current minus delta
(next-row delta) -> sets the selected row to current plus delta
(prev-col delta) -> sets the selected col to current minus delta
(next-col delta) -> sets the selected col to current plus delta

(set-selected-row! new-row) -> sets the selected row value
(set-selected-col! new-column) -> sets the selected col value
(select-id! id) -> sets selection cell and adjusts screen boundaries
(select cell-name) -> sets the selection cell based on cell name.
(adjust-screen-boundaries) -> sets the page for what to display
(set-corner-row! new-row) -> sets row of screen corner cell
(set-corner-col! new-column) -> sets the col of the screen corner cell

Load
----
(spreadsheet-load filename) -> loads file and runs each command from that file

Put
---
(put formula . where) -> determines which helper to call to fulfill put
(put-formula-in-cell formula cell-id) -> (put-expr (pin-down formula id) id)
(put-all-cells-in-row formula row) ->
(put-all-cells-in-col formula col) ->


Pin Down Formulas into Expressions
----------------------------------
(pin-down formula id) ->
(bound-check form) ->
(pin-down-cell args reference-id) ->
(pin-down-col new old) ->
(pin-down-row new old) ->

Dependency Management
---------------------
(put-expr expr-or-out-of-bounds id) -> puts an expression in a cell, removes cell from previous parents, sets the expression used to generate the cell, sets dependecies to see it as a parent, sets new parents to hold it as a child, and compute the new value for the cell.

Evaluating Expressions
----------------------
(ss-eval expr) -> converts an expression to it's value
*the-functions* -> list of lists (where first element is the name of a function, and the second element is the function.)

Print Screen
------------
(print-screen) -> void, displays the determined grid/selection
(show-column-lables col-number) -> void, displays the column labels
(display-cell-name id) -> void, displays the cell-name, like a1
(show-label to-go col) -> shows a column label, like _____a_____
(show-rows to-go col row)
(show-row to-go col row)
(selected-indices? col row) -> checks if a col/row together identify the selected Cell
(display-value val) -> uses align to truncate if necessary.
(display-expression expr) -> displays an expression
(display-invocation expr) -> displays an invocation (when a function is applied)

Cell Names
----------
(cell-name? expr) -> bool. Assumes that (first expr) is a letter and (bf expr) is a number.
(cell-name-column cell-name) -> gets the col number from the cell name
(cell-name-row cell-name) -> gets the row number from the cell name
(cell-name->id cell-name) -> gets an Id by name

Cell Ids
---------
(make-id col row) -> Id
(id-column id) -> col
(id-row id)-> row
(id? x) -> bool

Cells
-----
(make-cell) -> Cell

(cell-structure id) -> Cell from spreadsheet
(cell-structure-from-indices col row) -> Cell from spreadsheet

(cell-value id) -> Cell[0]
(cell-expr id) -> Cell[1]
(cell-parents id) -> Cell[2]
(cell-children id) -> Cell[3]

(set-cell-value! id val) -> void
(set-cell-expr! id val) -> void
(set-cell-parents! id val) -> void
(set-cell-children! id val) -> void

*the-spreadsheet-array* -> Vector of Vectors (of Cells)

(global-array-lookup col row) -> Cell

(init-array) -> void

(fill-array-with-rows n) -> void

(fill-row-with-cells vec n) -> void

Utilities
---------
(alphabet) -> Vector of lowercase letters
(letter? x) -> bool
(number->letter num) -> index into alphabet to get letter
(letter->number letter) -> find given letter, return counter index
(vector->member thing vector) -> index of thing in vector
(remdup lst) -> lst with duplicates removed
(rmv bad-item lst) -> remove bad-item from lst

Constants
---------
*the-spreadsheet-array*
total-cols
total-rows
