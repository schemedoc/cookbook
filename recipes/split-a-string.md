## Problem
Split a string into substrings at known delimiters.

## Solution

```Scheme
(define (string-split char-delimiter? string)
  (define (maybe-add a b parts)
    (if (= a b) parts (cons (substring string a b) parts)))
  (let ((n (string-length string)))
    (let loop ((a 0) (b 0) (parts '()))
      (if (< b n)
          (if (not (char-delimiter? (string-ref string b)))
              (loop a (+ b 1) parts)
              (loop (+ b 1) (+ b 1) (maybe-add a b parts)))
          (reverse (maybe-add a b parts))))))
```
Credit [Lassi Kortela](https://github.com/lassik) 

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