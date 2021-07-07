# Convert integer to list of digits

## Problem

You have an integer, and you want a list of all its base-10 digits as
integers.

If the integer is negative, the list should start with a minus sign.

## Solution

### Using number->string

```
(define (integer->list integer)
  (let ((chars (string->list (number->string integer))))
    (map (lambda (char)
           (string->number (string char)))
         chars)))
```

Credit [Jakub T. Jankiewicz](https://jcubic.pl/me)

## Usage

```
(integer->list 12345)
;; ==> '(1 2 3 4 5)
```
