# Test if list is ordered by comparator

## Problem

I have a list and I want to check if it's ordered or sorted using a functions

## Solution

It uses functions:

* `every` from [Map over n consecutive elements inside a list](/test-if-all-items-pass-predicate/)
* `seq-map` from [Map over n consecutive elements inside a list](/map-over-sequance-of-elements-inside-list/)

```scheme
(define (sorted? predicate lst)
  (let ((result (seq-map 2 predicate lst)))
    (every identity result)))
```

Credit: [Jakub T. Jankiewicz](https://jcubic.pl/me)

## Usage

```scheme
(sorted? < '(1 2 3 4))
;; ==> #t
(sorted? < '(1 3 3 4))
;; ==> #f
(sorted? >= '(4 3 3 1))
;; ==> #t
```
