# Check if all elements are equal

## Problem

I want to check if all arguments are equal, by default `eq?`, `eqv?`, and `equal?` accepts only two arguments.

## Solution

The code use procedure:
* `sublist-reduce` from recipe: [Fold over n consecutive elements inside a list](/fold-over-sequance-of-elements-inside-list/)

```scheme
(define (same? . args)
  (sublist-reduce 2 (lambda (a b result)
                      (and result (eq? a b)))
                  #t
                  args))
```

And here is alernative that exit early when any pair return `#f`.

```scheme
(define (same? . args)
  (call/cc
   (lambda (return)
     (sublist-reduce 2 (lambda (a b result)
                         (let ((eq (eq? a b)))
                           (if (not eq)
                               (return eq)
                               eq)))
                     #t
                     args))))
```

Credit: [Jakub T. Jankiewicz](https://jcubic.pl/me)

## Usage

```scheme
(same? 10 10 10)
;; ==> #t
(same? 10 10 10 20 30 40)
;; ==> #f
```

