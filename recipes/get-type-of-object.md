# Get type of object

## Problem

You need a procedure that will return the type of the object as a string.

## Solution

```Scheme
(define (type obj)
  (cond ((number? obj) "number")
        ((pair? obj) "pair")
        ((null? obj) "nil")
        ((string? obj) "string")
        ((symbol? obj) "symbol")
        ((vector? obj) "vector")
        ((procedure? obj) "procedure")
        ((port? obj)
         (cond ((input-port? obj) "input-port")
               ((output-port? obj) "output-port")
               (else "unknown-port")))
        ((eof-object? obj) "eof")
        ((char? obj) "character")
        ((boolean? obj) "boolean")))
```

Credit: [Jakub T. Jankiewicz](https://jcubic.pl/me)

## Usage

```Scheme
(type "foo")
;; ==> "string"

(type #\x)
;; ==> "character"

(type (current-input-port))
;; ==> "input-port"
```
