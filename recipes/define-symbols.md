# Define symbols

## Problem

I want to define a set of constants whose values are symbols with the
same name as the constant.

For example, `(make-symbols x y z)` should define `x`, `y` and `z`
which evaluate to the symbols `x`, `y` and `z` respectively.

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

(list x y z)
;; ==> (x y z)

(every symbol? (list x y z))
;; ==> #t

(map symbol->string (list x y z))
;; ==> ("x" "y" "z")
```
