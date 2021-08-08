# Find depth of list

## Problem

I want to calculate the depth of the list (how much the list is nested).

## Solution

```scheme
(define (depth a)
  (if (pair? a)
      (+ 1 (apply max (map depth a)))
      0))
```

Credit: [Nils M Holm](http://t3x.org/) (ref: [depth.scm](http://t3x.org/s9fes/depth.scm.html))

## Usage

```scheme
(depth '(a (b (c d (e)))))
;; ==> 4
```
