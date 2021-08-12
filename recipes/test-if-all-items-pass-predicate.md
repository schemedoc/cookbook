# Test if all items in list pass the predicate

## Problem

I have a list and I want to check if function return true for all items.

## Solution

```scheme
(define (every fn list)
  (if (null? list)
      #t
      (and (fn (car list)) (every fn (cdr list)))))
```

Credit: [Jakub T. Jankiewicz](https://jcubic.pl/me)

## Usage

```scheme
(every number? '(1 2 3 4))
;; ==> #t
(every number? '(1 foo 3 4))
;; ==> #f
```
