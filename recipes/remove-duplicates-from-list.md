# Remove duplicates from list

## Problem

You have a list and you want to remove any duplicated elements from
the list and the order should not change. The solution can use the
`equal?` procedure or use the comparison procedure as an argument.

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

Credit: [Lassi Kortela](https://github.com/lassik)

### Removing only adjacent duplicates

Here's a version that removes only duplicates that are adjacent (i.e.
next to each other). It doesn't need to use a hash table or other
memory.

```Scheme
(define (delete-adjacent-duplicates xs)
  (let loop ((prev-pair #f) (xs xs) (new-list '()))
    (if (null? xs)
        (reverse new-list)
        (loop xs
              (cdr xs)
              (let ((x (car xs)))
                (if (and prev-pair (equal? x (car prev-pair)))
                    new-list
                    (cons x new-list)))))))
```

Credit: [Lassi Kortela](https://github.com/lassik)

## Usage

```Scheme
(define numbers '(1 2 3 1 2 4 4 5 6 7 5))

(delete-duplicates numbers)
;; ==> (1 2 3 4 5 6 7)

(delete-adjacent-duplicates numbers)
;; ==> (1 2 3 1 2 4 5 6 7 5)
```
