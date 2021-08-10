# Compute Pascal's triangle

## Problem

Display the top `n` rows of
[Pascal's triangle](https://en.wikipedia.org/wiki/Pascal's_triangle).

## Solution

```Scheme
(define (map-iota proc limit)
  (let loop ((i 0) (result '()))
    (if (< i limit)
        (loop (+ i 1) (cons (proc i) result))
        (reverse result))))

(define (pascal n k)
  (cond ((or (= k 0) (= k n))
         1)
        ((< 0 k n)
         (+ (pascal (- n 1) (- k 1))
            (pascal (- n 1) k)))
        (else
         (error "Bad indexes" n k))))

(define (pascal-row n)
  (map-iota (lambda (k) (pascal n k))
            (+ n 1)))

(define (pascal-triangle number-of-rows)
  (map-iota pascal-row number-of-rows))
```

Credit: [Lassi Kortela](https://github.com/lassik)

## Usage

```Scheme
(pascal-triangle 10)
;; => ((1)
;;     (1 1)
;;     (1 2 1)
;;     (1 3 3 1)
;;     (1 4 6 4 1)
;;     (1 5 10 10 5 1)
;;     (1 6 15 20 15 6 1)
;;     (1 7 21 35 35 21 7 1)
;;     (1 8 28 56 70 56 28 8 1)
;;     (1 9 36 84 126 126 84 36 9 1))
```
