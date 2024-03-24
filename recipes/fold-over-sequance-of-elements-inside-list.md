# Fold over n consecutive elements inside a list

## Problem

This is similar problem to [Map over n consecutive elements inside a list](/map-over-sequance-of-elements-inside-list/)

But this time I want to fold a list of elements into a single value, using more than one element at the time.

e.g.:

for list `(1 2 3 4 5)` and `N = 2` it will call:

```scheme
(fn 4 5 (fn 3 4 (fn 2 3 (fn 1 2 <init>))))
```

## Solution

The code use function:
* `take` from recipe: [Select first n elements from list](/select-first-n-elements-from-list/)

```scheme
(define (sublist-reduce n fn init lst)
  (let loop ((lst lst) (result init))
    (if (< (length lst) n)
        result
        (let* ((next-list (take lst n))
               (args (append! next-list (list result))))
          (loop (cdr lst) (apply fn args))))))
```

The other name for this procedure could be `sublist-fold-right` anlalogous to fold-right method from
[SRFI-1]()

Credit: [Jakub T. Jankiewicz](https://jcubic.pl/me)

## Usage

```scheme
(sublist-reduce 2 (lambda (a b) (< a b)) #t '(1 2 3 4))
;; ==> #t
(sublist-reduce 3 (lambda (a b c result)
                    (and result (= a b c)))
                #t
                '(2 2 2 3 3 3 4 4 4))
;; ==> #f
```
