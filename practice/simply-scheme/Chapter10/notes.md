Terms
-----
(data structure: A way of organizing several pieces of information into a big chunk.)
(semipredicate: A function that returns the result if true, and false otherwise. Reference to Chapter 6)

Notes
-----
* High level design, ignoring arguments, was how the first version was presented.

Documentation
-------------
position: In the form of '_________
triple: An encoding of the rows, columns, and diagonals used for ttt.
    1 | 2 | 3
    4 | 5 | 6
    7 | 8 | 9 -> '(123 456 789 147 258 369 159 357)

find-triples: Takes a position and returns the game triples with the letters substituted in. Numbers indicate free spaces.

    >(find-triples '_xo_x_o__)
    '("1xo" "4x6" o89 "14o" xx8 o69 "1x9" oxo)

substitute-triple: Takes a combination (specific triple) and a position, and returns the triple with the moves applied to it.

    >(substitute-triple 456 '_xo_x_o__)
    "4x6"

substitute-letter: Takes a square (ttt sense) and a

my-pair?: Takes a triple and 'x or 'o. and returns a bool, true if 2 of the tripe are for the character and if the opponent doesn't have any appearances in the triple.

    > (my-pair? 'oo3 'o)
    #t

opponent: Takes a letter and returns the opponent for that letter, 'x for 'o, and vice versa.

    > (opponent 'o)
    'x

ttt: Takes a position and the letter representing the current player. Returns the position for that player to pick as a best move.
    > (ttt 'xx3______ 'x)
    3

ttt-choose: Takes the list of triples for ttt and the letter of the current player. Returns the position for that player to pick as a best move. This is the actual implementation of the strategy used to determine the best move.

    > (ttt-choose '(xx3 456 789 x47 x58 369 x59 357) 'x)
    3

i-can-win?: Takes the list of triples for ttt and the letter of the current player. Returns the position to win with if any of the triples only need one more of the letter to win, otherwise returns false.

    > (i-can-win? '(xx3 456 789 x47 x58 369 x59 357) 'x)
    3

    > (i-can-win? '(xx3 456 789 x47 x58 369 x59 357) 'o)
    #f
