# Split list into groups that are equal judging by a procedure

## Problem

Write a procedure `(group-by f lst)` that splits the list `lst` into
sublists. The sublist boundaries are determined by the value of `(f x)`
for each element `x` in `lst`. When the value changes, a new sublist
is started from that element. Values of `(f x)` are compared by
`equal?`.

Return a list of the groups, i.e. each group is a sublist in the main
list.

## Solution

```
(define (group-by f lst)
  (if (null? lst) '()
      (let ((first (car lst)))
        (let loop ((lst (cdr lst)) (key (f first)) (g (list first)) (gs '()))
          (if (null? lst)
              (reverse (cons (reverse g) gs))
              (let ((newkey (f (car lst))))
                (if (equal? key newkey)
                    (loop (cdr lst) key
                          (cons (car lst) g)
                          gs)
                    (loop (cdr lst) newkey
                          (list (car lst))
                          (cons (reverse g) gs)))))))))
```

Credit: [Lassi Kortela](https://github.com/lassik)

## Usage

```
(group-by even? (iota 10))
;; ==> ((0) (1) (2) (3) (4) (5) (6) (7) (8) (9))
```

```
(group-by odd? '(1 3 5 2 1 6 4 1 7))
;; ==> ((1 3 5) (2) (1) (6 4) (1 7))
```

```
(group-by string-length '("aa" "bb" "ccc" "dddd" "eeee" "ffff" "g" "h"))
;; ==> (("aa" "bb") ("ccc") ("dddd" "eeee" "ffff") ("g" "h"))
```

```
(group-by (lambda (i) (truncate-quotient i 3)) (iota 20))
;; ==> ((0 1 2) (3 4 5) (6 7 8) (9 10 11) (12 13 14) (15 16 17) (18 19))
```
