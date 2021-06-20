## Problem
You have a number with more than one digits, and you want a list with all its digits as numbers.

## Solution
```scheme
(define (number->list number)
  (let ((chars (string->list (number->string number))))
    (map (lambda (char)
           (string->number (string char)))
         chars)))
```
Credit [Jakub T. Jankiewicz](https://jcubic.pl/me)

## Usage
```scheme
(number->list 10001)
;; ==> '(1 0 0 0 1)
```
