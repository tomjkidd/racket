Analysis
--------

Which procedures adjust screen boundaries?
* adjust-screen-boundaries
* set-corner-row!
* set-screen-corner-cell-id!

Currently, the only thing that changes the window is the select command.
It does so using the adjust-screen-boundaries procedure, which use the set-corner-row! and set-corner-column! procedures to ultimately call set-screen-corner-cell-id!.

These procedures just set up the screen-corner-cell-id, which will be used by print-screen to determine how to display the page, relative to this corner cell.

To add the new functionality, all that needs to happen is to add a new
command to the command definitions, and have that command set a new corner
cell. The print-screen function should take care of the rest.

Design
------
The goal is to design commands to move the window of cells displayed on the screen without changing the selected cell.

The window command can be used for this purpose.
Method 1:
    (window [col] [row])
Shift window by [col] columns. Positive shifts right, negative shifts left
Shift window by [row] columns. Positive shifts down, negative shifts up

Method 2:
    (window cell-name)
Puts the cell identified by cell-name in the top-left corner, if possible.
