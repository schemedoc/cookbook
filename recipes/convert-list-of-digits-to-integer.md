# Convert list of digits into integer

## Problem

I have a list of integers that are digits of a number, the digits may start
with symbol `-`. So this is the reverse operation of the function `integer->list`
from recipe [Convert integer to list of digits](/convert-integer-to-list-of-digits/).

## Solution

### Using string->number and number->string

```scheme
(define (list->integer lst)
  (string->number (apply string-append
                         (map (lambda (item)
                                (if (and (symbol? item) (symbol=? item '-))
                                    "-"
                                    (number->string item)))
                              lst))))
```

Credit: [Jakub T. Jankiewicz](https://jcubic.pl/me)

## Usage

```scheme
(list->integer '(- 1 2 3 4))
;; ==> -1234
(list->integer '(1 0 0 0 0 1))
;; ==> 100001
```

Testing if the both functions return original value

```scheme
(let ((numbers '(1000 -1234)))
  (for-each (lambda (number)
              (display (eq? number (list->integer (integer->list number))))
              (newline))
            numbers))
;; ==> #t
;; ==> #t
```
