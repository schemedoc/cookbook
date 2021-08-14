# Select first n elements from list

## Problem

I want to select first elements from list.

## Solution

```schem
(define (take lst n)
  (let loop ((result '()) (i n) (lst lst))
    (if (or (null? lst) (<= i 0))
        (reverse result)
        (loop (cons (car lst) result) (- i 1) (cdr lst)))))
```

Credit: [Jakub T. Jankiewicz](https://jcubic.pl/me)

### SRFI
The same function is part of the [SRFI-1](https://srfi.schemers.org/srfi-1/srfi-1.html#take).

## Usage
```scheme
(take '(1 2 3 4 5) 3)
;; ==> (1 2 3)
```
