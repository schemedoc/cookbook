# Compute Pascal's triangle

## Problem

Display the top `n` rows of
[Pascal's triangle](https://en.wikipedia.org/wiki/Pascal's_triangle).

## Solution

```Scheme
(define (pascal n k)
  (cond ((or (= k 0) (= k n))
         1)
        ((< 0 k n)
         (+ (pascal (- n 1) (- k 1))
            (pascal (- n 1) k)))
        (else
         (error "Bad indexes" n k))))

(define (display-triangle rows)
  (let outer ((n 0))
    (when (< n rows)
      (let inner ((k 0))
        (cond ((<= k n)
               (display (pascal n k))
               (display " ")
               (inner (+ k 1)))
              (else
               (newline)
               (outer (+ n 1))))))))
```

Credit: [Lassi Kortela](https://github.com/lassik)

## Usage

```Scheme
(display-triangle 10)
;; 1
;; 1 1
;; 1 2 1
;; 1 3 3 1
;; 1 4 6 4 1
;; 1 5 10 10 5 1
;; 1 6 15 20 15 6 1
;; 1 7 21 35 35 21 7 1
;; 1 8 28 56 70 56 28 8 1
;; 1 9 36 84 126 126 84 36 9 1
```
