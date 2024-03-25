# Test if list is ordered by comparator

## Problem

I have a list and I want to check if it's ordered or sorted using a functions

## Solution

It uses functions:

* `every` from [Map over n consecutive elements inside a list](/test-if-all-items-pass-predicate/)
* `sublist-map` from [Map over n consecutive elements inside a list](/map-over-sequance-of-elements-inside-list/)

```scheme
(define (ordered? predicate lst)
  (let ((result (sublist-map 2 predicate lst)))
    (every identity result)))
```

Credit: [Jakub T. Jankiewicz](https://jcubic.pl/me)

## Usage

```scheme
(ordered? < '(1 2 3 4))
;; ==> #t
(ordered? < '(1 3 3 4))
;; ==> #f
(ordered? >= '(4 3 3 1))
;; ==> #t
```
