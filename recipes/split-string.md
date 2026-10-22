# Split string

## Problem

Split a string into substrings at known delimiters.

## Solution

```Scheme
(define (string-split char-delimiter? string)
  (define (finish chars parts)
    (if (null? chars) parts (cons (list->string (reverse chars)) parts)))
  (let loop ((cs (string->list string)) (chars '()) (parts '()))
    (cond
      ((null? cs)
       (reverse (finish chars parts)))
      ((char-delimiter? (car cs))
       (loop (cdr cs) '() (finish chars parts)))
      (else
       (loop (cdr cs) (cons (car cs) chars) parts)))))
```

`string-ref` and `substring` are only guaranteed O(1) and O(k)
respectively by the Scheme standards when a string is represented as
an array of characters. Some implementations represent strings
internally as, e.g., UTF-8 byte sequences, where both operations can
be O(n). Driving the loop with repeated `string-ref` calls (as in an
earlier version of this recipe) can then make the whole function
accidentally quadratic. `string->list` walks the string exactly once
regardless of its internal representation, and `list->string` builds
each substring in time proportional to its own length, keeping the
whole split linear.

Credit: [Lassi Kortela](https://github.com/lassik)

## Usage

```Scheme
(string-split char-whitespace? "")
;; ==> ()

(string-split char-whitespace? " \t  ")
;; ==> ()

(string-split char-whitespace? "ab")
;; ==> ("ab")

(string-split char-whitespace? "ab   c  d \t efg ")
;; ==> ("ab" "c" "d" "efg")
```
