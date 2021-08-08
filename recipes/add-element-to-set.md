## Problem

You want to add element to the list, but make all elements unique. So it work like set.

## Solution
    
```scheme
(define (adjoin x a)
  (if (member x a)
      a
      (cons x a)))
```

Credit: [Nils M Holm](http://t3x.org/) (ref: [adjoin.scm](http://t3x.org/s9fes/adjoin.scm.html))

## Usage

```scheme
(adjoin 'x '(a b c))
;; ==> (x a b c)
(adjoin 'c '(a b c))
;; ==> (a b c)
```