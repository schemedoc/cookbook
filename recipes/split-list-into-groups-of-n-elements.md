# Split list into groups of N elements

## Problem

Write a procedure `(group n lst)` that splits a list into groups of
`n` consecutive elements.

If the list length is not divisible by `n`, the last group will have
fewer than `n` elements.

The procedure should return a list of the groups in order, such that
each group is a sublist of the main list.

## Solution

```Scheme
(define (group n lst)
  (if (< n 1)
      (error "group: n must be positive" n)
      (let loop ((lst lst) (m n) (g '()) (gs '()))
        (cond ((and (null? lst) (null? g))
               (reverse gs))
              ((or (null? lst) (zero? m))
               (loop lst n '() (cons (reverse g) gs)))
              (else
               (loop (cdr lst) (- m 1) (cons (car lst) g) gs))))))
```

Credit: [Lassi Kortela](https://github.com/lassik)

## Usage

### Normal cases

```Scheme
(group 1 (iota 10))
;; ==> ((0) (1) (2) (3) (4) (5) (6) (7) (8) (9))

(group 2 (iota 10))
;; ==> ((0 1) (2 3) (4 5) (6 7) (8 9))

(group 3 (iota 10))
;; ==> ((0 1 2) (3 4 5) (6 7 8) (9))

(group 4 (iota 10))
;; ==> ((0 1 2 3) (4 5 6 7) (8 9))

(group 5 (iota 10))
;; ==> ((0 1 2 3 4) (5 6 7 8 9))

(group 6 (iota 10))
;; ==> ((0 1 2 3 4 5) (6 7 8 9))
```

### Special cases

```Scheme
(group 20 (iota 10))
;; ==> ((0 1 2 3 4 5 6 7 8 9))

(group 1 '())
;; ==> ()

(group 2 '())
;; ==> ()
```
