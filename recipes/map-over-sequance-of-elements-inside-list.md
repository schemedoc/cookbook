# Map over n consecutive elements inside a list

## Problem

I have a list of elements, and I want to map using a function when on
each loops the function gets n items from list.

e.g.:

for list `(1 2 3 4 5)` and `N = 2` it will call

```scheme
(fn 1 2)
(fn 2 3)
(fn 3 4)
(fn 4 5)
```

and collect the results into single list.

## Solution

```scheme
(define (seq-map n fn seq-list)
  (let loop ((seq-list seq-list) (result '()))
    (if (null? seq-list)
        (reverse result)
        (let ((next-list (take n seq-list)))
          (loop (cdr seq-list) (cons (apply fn next-list) result))))))
```

Credit: [Jakub T. Jankiewicz](https://jcubic.pl/me)

## Usage

```scheme
(seq-map 2 < '(1 2 3 4))
;; ==> (#t #t #t)
```
