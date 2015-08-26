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
