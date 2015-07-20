Terms
-----
(lambda: special form that generates procedures)
(named function: f(x) = 1 + 2, name is f)
(unnamed function: (x -> 3x + 8), no name)
(abstraction: the idea of naming something and forgetting the details of its implementation)

Notes
-----
* (lambda (<arguments>) (<body>))
* lambda returns a function
* Comes from branch of mathematical logic called "lambda calculus"
* lambda allows you to create a function without a name, an anonymous function.
* Use substitution model to resolve arguments
* Truth about define:
  (define (square x) (* x x)) is equivalent to...
  (define square (lambda (x) (* x x)))
* Truth about let: let is merely an abbreviation for creating and invoking an anonymous procedure.
* Use a lambda when you want to return a function.
