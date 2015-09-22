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
