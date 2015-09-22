#lang racket

(require "../full-core.rkt")

(set? '(apple peaches pears plums))
(set? '())
(set? '(apple 3 pear 4 9 apple 3 4))

(makeset '(apple peach pear peach plum apple lemon peach))
(makeset '(apple 3 pear 4 9 apple 3 4))
(subset? '(5 chicken wings)
         '(5 hamburgers 2 pieces fried chicken and light duckling wings))
(subset? '(4 pounds of horseradish)
         '(four pounds chicken and 5 ounces horseradish))

(eqset? '(6 large chickens with wings)
        '(6 chickens with large wings))

(intersect? '(stewed tomatoes and macaroni)
            '(macaroni and cheese))

(intersect '(stewed tomatoes and macaroni)
           '(macaroni and cheese))

(union '(stewed tomatoes and macaroni casserole)
       '(macaroni and cheese))

(difference '(a b c d) '(a b c))

(intersectall (list '(a b c)
                    '(c a d e)
                    '(e f g h a b)))

(intersectall (list '(6 pears and)
                    '(3 peaches and 6 peppers)
                    '(8 pears and 6 plums)
                    '(and 6 prunes with some apples)))

(pair? '(pair pear))
(pair? '(3 7))
(pair? (list '(2) '(pair)))

(rel? '(apples peaches pumpkin pie))

(rel? (list '(apples peaches)
            '(pumpkin pie)
            '(apples peaches)))

(rel? (list '(apples peaches)
            '(pumpkin pie)))

(rel? (list '(4 3)
            '(4 2)
            '(7 6)
            '(6 2)
            '(3 4)))

(fun? (list '(4 3)
            '(4 2)
            '(7 6)
            '(6 2)
            '(3 4)))

(fun? (list '(8 3)
            '(4 2)
            '(7 6)
            '(6 2)
            '(3 4)))

(fun? (list '(d 4)
            '(b 0)
            '(b 9)
            '(e 5)
            '(g 4)))

(revrel (list '(8 a)
              '(pumpkin pie)
              '(got sick)))

(fullfun? (list '(8 3)
                '(4 2)
                '(7 6)
                '(6 2)
                '(3 4)))

(fullfun? (list '(8 3)
                '(4 8)
                '(7 6)
                '(6 2)
                '(3 4)))

(fullfun? (list '(grape raisin)
                '(plum prune)
                '(stewed prune)))

(fullfun? (list '(grape raisin)
                '(plum prune)
                '(stewed grape)))

(firsts (list '(8 3)
              '(4 8)
              '(7 6)
              '(6 2)
              '(3 4)))

(seconds (list '(8 3)
               '(4 8)
               '(7 6)
               '(6 2)
               '(3 4)))

(one-to-one? (list '(chocolate chip)
                   '(coughy cookie)))

