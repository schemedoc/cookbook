# Map over n consecutive elements inside a list

## Problem

I have a list of elements, and I want to map using a function when on
each loop the function gets n items from a list.

e.g.:

for list `(1 2 3 4 5)` and `N = 2` it will call:

```scheme
(fn 1 2)
(fn 2 3)
(fn 3 4)
(fn 4 5)
```

for list '(1 2 3 4 5) and `N = 3` it will call:

```scheme
(fn 1 2 3)
(fn 2 3 4)
(fn 3 4 5)
```

and collect the results into a single list.

## Solution

The code use function:
* `take` from recipe: [Select first n elements from list](/select-first-n-elements-from-list/)

```scheme
(define (sublist-map n fn lst)
  (let loop ((lst lst) (result '()))
    (if (< (length lst) n)
        (reverse result)
        (let ((next-list (take lst n)))
          (loop (cdr lst) (cons (apply fn next-list) result))))))
```

**NOTE:** This looping overlap the list, so window is always moving one element per loop.
If you want the whole window to move to the next n elements you can use this solution instead:

```scheme
(define (group-map n fn seq-list)
  (map (lambda (lst)
         (apply fn lst))
       (group n seq-list)))
```

Above function uses:
* function `group` from recipe: [Split list into groups of N elements](/plit-list-into-groups-of-n-elements/)

Credit: [Jakub T. Jankiewicz](https://jcubic.pl/me)

## Usage

```scheme
(sublist-map 2 < '(1 2 3 4))
;; ==> (#t #t #t)
(sublist-map 3 = '(2 2 2 3 3 3 4 4 4))
;; ==> (#t #f #f #t #f #f #t)

(group-map 3 = '(2 2 2 3 3 3 4 4 4))
;; ==> (#t #t #t)
(group-map 3 = '(2 2 2 0 1 2 4 4 4))
;; ==> (#t #f #t)
(group-map 3 + '(2 2 2 3 3 3 4 4 4))
;; ==> (6 9 12)
(group-map 3 max '(1 2 3 4 5 6 7 8 9))
;; ==> (3 6 9)
```
