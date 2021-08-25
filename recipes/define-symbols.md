# Define symbols

## Problem

I want to define several variables that have their symbol as value.
For example `(make-symbols x y z)` should define `x`, `y` and `z`
which evaluate to `x`, `y` and `z` respectively.

## Solution

```scheme
(define-syntax make-symbols
  (syntax-rules ()
    ((_ symbol ...)
     (begin
       (define symbol 'symbol) ...))))
```

Credit: [Vasilij Schneidermann](https://depp.brause.cc/)

## Usage

```scheme
(make-symbols x y z)
(display (list x y z))
(newline)
;; ==> (x y z)
```
