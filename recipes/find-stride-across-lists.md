# Find stride across lists

## Problem

I store a matrix as a list of rows, where each row is a list of column
values for that row. I want to check if there is a column in the
matrix that satisfies a comparator `p`.

`p` can actually return any non-`#f` value and that value is returned
as-is. And the lists don't actually have to represent a matrix: the
rows don't all need to have the same number of columns.

## Solution

```scheme
(define (find-stride p . a*)
  (letrec
    ((car-of
       (lambda (a)
         (map car a)))
     (cdr-of
       (lambda (a)
         (map cdr a)))
     (any-null
       (lambda (a)
         (memq '() a)))
     (find*
       (lambda (a*)
         (and (not (any-null a*))
              (or (apply p (car-of a*))
                  (find* (cdr-of a*)))))))
    (find* a*)))
```

Credit: [Nils M. Holm](https://t3x.org) (ref: [exists.scm](https://t3x.org/s9fes/exists.scm.html))

## Usage

```scheme
(find-stride < '(9 1) '(8 2) '(7 3))
;; ==> #t  ; because (< 1 2 3)
```
