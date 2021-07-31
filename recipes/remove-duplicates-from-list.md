# Remove duplicates from list

## Problem

You have a list and you want to remove any duplicated elements from
the list and the order should not change. The solution can use the
`equal?` function or use the comparison function as an argument.

## Solution

### Using SRFI

There's a
[`delete-duplicates`](https://srfi.schemers.org/srfi-1/srfi-1.html#delete-duplicates)
procedure in SRFI 1.

### Using hash tables (SRFI 125)

```Scheme
(define (delete-duplicates xs)
  (let ((seen (make-hash-table equal?)))
    (let loop ((xs xs) (new-list '()))
      (if (null? xs)
          (reverse new-list)
          (loop (cdr xs)
                (let ((x (car xs)))
                  (if (hash-table-contains? seen x)
                      new-list
                      (begin (hash-table-set! seen x #t)
                             (cons x new-list)))))))))
```

Credit [Lassi Kortela](https://github.com/lassik)

## Usage

```Scheme
(delete-duplicates '(1 2 3 1 2 4 4 5 6 7 5))
;; ==> (1 2 3 4 5 6 7)
```
