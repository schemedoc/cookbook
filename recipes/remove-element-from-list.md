# Remove element from list

## Problem

You have a list and want to remove elements that return true for a
given predicate function.

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

### Using SRFI

SRFI 1 has a `remove` procedure that works as above. It also has a
`filter` procedure that is the opposite: if the predicate returns
`true` the element will be kept in the resulting list.

## Usage

```Scheme
(define >10 (lambda (x) (> x 10)))
(remove >10 '(1 2 3 4 10 11 12 13 14))
;; ==> (1 2 3 4 10)
```
