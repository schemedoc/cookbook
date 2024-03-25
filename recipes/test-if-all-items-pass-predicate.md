# Test if all items in list pass the predicate

## Problem

I have a list and I want to check if function return true for all items.

## Solution

```scheme
(define (every? fn list)
  (or (null? list)
      (and (fn (car list)) (every? fn (cdr list)))))
```

Credit: [Jakub T. Jankiewicz](https://jcubic.pl/me)

## SRFI

[SRFI-1](https://srfi.schemers.org/srfi-1/) has function `every`, but `every?` always return `#f` or `#f` that's why it has question mark.

## Usage

```scheme
(every? number? '(1 2 3 4))
;; ==> #t
(every? number? '(1 foo 3 4))
;; ==> #f
(every? number? '())
;; ==> #t
(every? string->number '("foo" "bar" "baz"))
;; ==> #f
```
