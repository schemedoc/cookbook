# Map over n consecutive elements inside a list

## Problem

I have a list of elements, and I want to map using a function when on
each loops the function gets n items from list.

e.g.:

for list `(1 2 3 4 5)` and `N = 2` it will call:

```scheme
(fn 1 2)
(fn 2 3)
(fn 3 4)
(fn 4 5)
(fn 5)
```

for list '(1 2 3 4 5) and `N = 3` it will call:

```scheme
(fn 1 2 3)
(fn 2 3 4)
(fn 3 4 5)
(fn 4 5)
(fn 5)
```

and collect the results into single list.

## Solution

The code use function:
* `take` from recipe: [Select first n elements from list](/select-first-n-elements-from-list/)

```scheme
(define (seq-map n fn seq-list)
  (let loop ((seq-list seq-list) (result '()))
    (if (null? seq-list)
        (reverse result)
        (let ((next-list (take seq-list n)))
          (loop (cdr seq-list) (cons (apply fn next-list) result))))))
```

**NOTE:** This looping overlap the list so window is always moving one element per loop.
If you want the whole window to move to next n elements you can use this solution instead:

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
(seq-map 2 < '(1 2 3 4))
;; ==> (#t #t #t)
(seq-map 3 = '(2 2 2 3 3 3 4 4 4))
;; ==> (#t #f #f #t #f #f #t #t #t)

(group-map 3 = '(2 2 2 3 3 3 4 4 4))
;; ==> (#t #t #t)
(group-map 3 = '(2 2 2 0 1 2 4 4 4))
;; ==> (#t #f #t)
```
