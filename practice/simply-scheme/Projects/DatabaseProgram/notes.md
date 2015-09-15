Terms
-----
(database: a large file with lots of related data in it)

(record: an organized collection of fields)

(field: a piece of information)

Initial Interface (psuedo-code where applicable)
-----------------
(new-db *db-name* *field-name-list*)

(insert) ->
    foreach field-name in *field-name-list*:
        display "Value for"
        display field-name
        display "-->"
        read user input
    then "Insert another?"

(list-db) ->
    display "Record"
    display record-number
    foreach field-name in *field-name-list*:
        display field-name
        display ":"
        display field-value
        newline
    display "listed"

(count-db) ->
    display length of database

(sort-on *field-name*) ->
    keep a reference to *field-name* in order to sort when (list-db) is called

(edit-record *record-number*) ->
    **Display the current record**
    foreach field-name in *field-name-list*:
        display fieldname

    display "Edit which field?"
    read user input
    if user-input == #f:
        stop
    else
        loop for edit a field
    display "Edited"

Implementation details
----------------------
database -> vector with three elements [name, field-name-list, record-list]
record -> vector with (vector-length field-name-list) elements
