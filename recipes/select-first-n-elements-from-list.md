# Select first n elements from list

## Problem

I want to select first elements from list.

## Solution

```schem
(define (take n lst)
  (let iter ((result '()) (i n) (lst lst))
    (if (or (null? lst) (<= i 0))
        (reverse result)
        (iter (cons (car lst) result) (- i 1) (cdr lst)))))
```

Credit: [Jakub T. Jankiewicz](https://jcubic.pl/me)

### SRFI

Function with same name, but with arguments swaped is part of the [SRFI-1](https://srfi.schemers.org/srfi-1/srfi-1.html#take).

## Usage

```scheme
(take 3 '(1 2 3 4 5))
;; ==> (1 2 3)
```
