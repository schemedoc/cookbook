## Problem

I want to be able to take random elements from a list without getting unwanted duplicates (so if I ask for 4 random elements from the list `'(a b c d)` I don't want to get to `'(d a ba )`, for example).

## Solution

```Scheme
(define (pick elements how-many)
  (if (< how-many 0)
      (error "Cannot create a list of negative length."
             how-many)
      (letrec* ((list-length (length elements))
                (new-index (lambda (iteration-list-length)
                             (random-integer (if (zero? iteration-list-length)
                                                 list-length
                                                 iteration-list-length))))
                (do-pick
                 (lambda (pick-from unchosen result index how-many)
                   (cond ((zero? how-many)
                          result)
                         ((null? pick-from)
                          (do-pick (if (null? unchosen)
                                       elements
                                       unchosen)
                                   '()
                                   result
                                   index
                                   how-many))
                         ((zero? index)
                          (do-pick (cdr pick-from)
                                   unchosen
                                   (cons (car pick-from)
                                         result)
                                   (new-index (+ (length (cdr pick-from))
                                                 (length unchosen)))
                                   (- how-many 1)))
                         (else
                          (do-pick (cdr pick-from)
                                   (cons (car pick-from)
                                         unchosen)
                                   result
                                   (- index 1)
                                   how-many))))))
        (do-pick elements '() '() (random-integer list-length) how-many))))
```
Credit [Tim Van den Langenbergh](http://tmtvl.info)

## Usage

```Scheme
(pick (iota 100)
	  100)
;; (79 28 18 40 90 29 30 66 80 36 23 34 42 84 25 35 88 54 15 92 69 8 2 16 95 27 19
;; 74 91 77 68 3 83 57 26 86 89 60 53 47 21 85 72 0 73 96 93 58 32 10 ....)
(pick '(A C G T)
         5)
;; (T G A T C)
(+ (car (pick (iota 6)
			  1))
   1)
;; 1
```
