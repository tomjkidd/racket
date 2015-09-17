#lang racket

(require "../core.rkt")

(rember 'mint '(lamb chops and mint jelly))
(rember 'mint '(lamb chops and mint flavored mint jelly))
(rember 'toast '(bacon lettuce and tomato))
(rember 'cup '(coffee cup tea cup and hick cup))

(rember 'and '(bacon lettuce and tomato))

#|
The Second Commandment: Use cons to build lists.
|#

(rember 'sauce '(soy sauce and tomato sauce))

(firsts (list '(apple peach pumpkin)
              '(plum pear cherry)
              '(grape raisin pea)
              '(bean carror eggplant)))
(firsts (list '(a b) '(c d) '(e f)))
(firsts '())
(firsts (list (list '(five plums) 'four)
              '(eleven green oranges)
              (list '(no) 'more)))
#|
The Third Commandment:
When building a list, describe the first typical element,
and then cons it onto the natural recursion.
|#
(insertR 'topping 'fudge '(ice cream with fudge for dessert))
(insertR 'jalapeno 'and '(tacos tamales and salsa))
(insertR 'e 'd '(a b c d f g d h))

(insertR 'topping 'fudge '(ice cream fudge for dessert))
(subst 'topping 'fudge '(ice cream with fudge for dessert))

(subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))

(multirember 'cup '(coffee cup tea cup and hick cup))
(multiinsertR 'fried 'fish '(chips and fish or fish and fried))
(multiinsertL 'fried 'fish '(chips and fish or fish and fried))
#|
The Fourth Commandment
Always change at least one argument while recurring.
It must be changed to be closer to termination.
The chainging argument must be tested in the termination condition:
when using cdr, test termination with null?
|#
(multisubst 'bean 'cup '(coffee cup tea cup and hick cup))