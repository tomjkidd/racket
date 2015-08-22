Terms
-----
(spreadsheet: 2-d information grid)
(cell: intersection of a row and column)
(portable: a program that works in all versions of Scheme)
(extensible: programmer has made it possible for the user to add capabilities
  to the program, or modify existing capabilities)

Notes
-----
* Prints entire new screen after each command
* columns: [a-z]
* rows: [1-30]
* value: number | quoted word
* number -> right align, 2 digits after decimal point
* word -> left align
* too wide -> 9 chars followed by +
* select something that is too wide to see it's full value
* lists are not allowed as cell values
* formulas based only on numbers

Spreadsheet Program Interface
-----------------------------
(spreadsheet) : used to start the program

Spreadsheet Program Commands
----------------------------
(put [value]) -> currently selected cell
(put [value] [cell]) -> put value in specific cell
(put ()) -> erase a value, including full row or col
(put [value] [row-number]) -> fill row with value, only vacant values change
(put [value] [col-letter]) -> fill col with value, only vacant values change
(put [formula] [cell]) -> fill cell with an expression for it's value
(put (* (cell b) (cell c)) d) -> specify a row-based formula
f -> Forward (right)
b -> Back (left)
n -> Next (down)
p -> Previous (up)
(f [num]) -> go in direction num times
(select [cell]) -> select a specific cell
(load "filename") -> load a command file, and run it
