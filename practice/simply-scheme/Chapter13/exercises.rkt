#lang racket
(require (planet dyoo/simply-scheme:2))
(require racket/trace)

(define (fib n)
  (if (<= n 2)
      1
      (+ (fib (- n 1))
         (fib (- n 2)))))

(trace fib)
(fib 4)

#! 13.1
(define (explode wd)
  (if (= (count wd) 1)
      wd
      (se
       (first wd)
       (explode (bf wd)))))

(trace explode)
(explode 'ape)
#|
>(explode 'ape)
> (explode 'pe)
> >(explode 'e)
< <'e
< '(p e)
<'(a p e)
|#

#! 13.2
(define (pigl wd)
  (if (member? (first wd) 'aeiou)
      (word wd 'ay)
      (pigl (word (bf wd) (first wd)))))
(trace pigl)
(pigl 'throughout)
#|
>(pigl 'throughout)
>(pigl 'hroughoutt)
>(pigl 'roughoutth)
>(pigl 'oughoutthr)
<'oughoutthray
|#

#! 13.3
(define (downup wd)
  (se wd (downup (bl wd)) wd))
(trace downup)
;;(downup 'toe) ;; Error -> When empty word is reached, (bl "") can't be done!
(define (downup-fixed wd)
  (if (<= (count wd) 1)
      wd
      (se wd (downup-fixed (bl wd)) wd)))
(trace downup-fixed)

#! 13.4
(define (forever n)
  (if (= n 0)
      1
      (+ 1 (forever n))))
(trace forever)
;;(forever 1)
#|
Here forever recurses infinitely because the argument is not changed when the
recursive call is made, it will never reach the base case.
|#

#! 13.5
(pigl 'prawn)
(downup-fixed 'smile)
#|
>(downup-fixed 'smile)
> (downup-fixed 'smil)
> >(downup-fixed 'smi)
> > (downup-fixed 'sm)
> > >(downup-fixed 's)
< < <'s
< < '(sm s sm)
< <'(smi sm s sm smi)
< '(smil smi sm s sm smi smil)
<'(smile smil smi sm s sm smi smil smile)

Suppose there is only one downup specialist. That specialist would have to remember
every intermediate result as they reduced the word. In each invocation, a letter is
lost, so the rearrangement is not as straight forward as for pigl.
|#

#! 13.6
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))
(trace factorial)
(factorial 6)
#|
When (factorial 2) is calculated, it was called due to the calculation of (factorial 3),
so it will pass the value along to allow (factorial 3) result to move to help resolve
the (factorial 4) call, and so on until (factorial 6).
|#