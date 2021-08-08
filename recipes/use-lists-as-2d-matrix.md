# Use lists as 2D matrix

## Problem

Create matrix data structure and functions to operate on matrices.

## Solution

```scheme
(define (make-matrix n)
  (let outter ((i n) (result '()))
    (if (= i 0)
        result
        (outter (- i 1)
                (cons
                 (let inner ((j n) (row '()))
                   (if (= j 0)
                       row
                       (inner (- j 1) (cons (if (= i j) 1 0) row))))
                 result)))))

(define (nth list n)
  (let loop ((n n) (result list))
    (if (= n 0)
        (car result)
        (loop (- n 1)
              (cdr result)))))

(define matrix-row nth)

(define (matrix-col M n)
  (let loop ((i (length M)) (result '()))
    (if (= i 0)
        result
        (loop (- i 1)
              (cons (nth (nth M (- i 1)) n) result)))))

(define (matrix-mul N M)
  (let outter ((i (length N)) (result '()))
    (if (= i 0)
        result
        (outter (- i 1)
                (cons
                 (let inner ((j (length (car M))) (row '()))
                   (if (= j 0)
                       row
                       (inner
                        (- j 1)
                        (cons (reduce + (map *
                                             (matrix-row N (- i 1))
                                             (matrix-col M (- j 1))))
                              row))))
               result)))))

(define (reduce fun lst)
  (let loop ((result (car lst)) (lst (cdr lst)))
    (if (null? lst)
        result
        (loop (fun result (car lst)) (cdr lst)))))

(define (matrix-vector-mul v M)
  (car (matrix-mul (list v) M)))

(define (matrix-transpose M)
  (let loop ((M M) (result '()))
    (if (null? (car M))
        result
        (loop (map cdr M) (append result (list (map car M)))))))

(define (matrix-sum N M)
  (map (lambda (nrow mrow) (map + nrow mrow)) N M))
```

Credit: [Jakub T. Jankiewicz](https://jcubic.pl) (ref: [Matrix manipulation in scheme](https://jcubic.wordpress.com/2011/09/29/matrix-manipulation-in-scheme/))

## Usage

```scheme
(define M1 '((1 2 3) (2 3 4) (3 2 1)))
(define M2 (make-matrix 3))

(matrix-mul M1 M2)
;; ==> ((1 2 3) (2 3 4) (3 2 1))

(matrix-mul M1 '((2 3 1) (1 2 1) (1 3 1)))
;; ==> ((7 16 6) (11 24 9) (9 16 6))

(matrix-sum M1 M2)
;; ==> ((2 2 3) (2 4 4) (3 2 2))

(matrix-vector-mul '(2 3 1) M1)
;; ==> (11 15 19)
```
