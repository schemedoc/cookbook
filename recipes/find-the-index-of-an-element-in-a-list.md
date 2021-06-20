## Problem
You have a list and a function and you want to find the first index for an item for which the function returns true.

## Solution:
```scheme
(define (index fn list)
  (let iter ((list list) (index 0))
    (if (null? list)
        -1
        (let ((item (car list)))
          (if (fn item)
              index
              (iter (cdr list) (+ index 1)))))))
```

Credit @jcubic

## Usage
```scheme
(define >10 (lambda (x) (> x 10)))
(index >10 '(1 2 3 4 10 11 12 13 14))
;; ==> 5
```