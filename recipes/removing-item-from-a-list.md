## Problem
You have a list and want to remove items that return true for a given predicate function

## Solution
```Scheme
(define (remove fn list)
  (let iter ((list list) (result '()))
    (if (null? list)
        (reverse result)
        (let ((item (car list)))
          (if (fn item)
              (iter (cdr list) result)
              (iter (cdr list) (cons item result)))))))
```
Credit [Jakub T. Jankiewicz](https://jcubic.pl/me)

### SRFI
This function is part of SRFI-1, which also has `filter` that is reverse of `remove`, where if the function returns `true` the item will be kept in the output list.

## Usage
```Scheme
(define >10 (lambda (x) (> x 10)))
(remove >10 '(1 2 3 4 10 11 12 13 14))
;; ==> (1 2 3 4 10)
```
