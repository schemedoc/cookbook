# Create K combinations from list

## Problem

I want to generate k-combinations from a given list.

## Solution

```scheme
(define (combine3 n set rest)
  (letrec
    ((tails-of
       (lambda (set)
         (cond ((null? set)
                 '())
               (else
                 (cons set (tails-of (cdr set)))))))
     (combinations
       (lambda (n set)
         (cond
           ((zero? n)
             '())
           ((= 1 n)
             (map list set))
           (else
             (apply append
                    (map (lambda (tail)
                           (map (lambda (sub)
                                  (cons (car tail) sub))
                                (combinations (- n 1) (rest tail))))
                         (tails-of set))))))))
    (combinations n set)))

;; create k-combination without repetion
(define (combine n set)
  (combine3 n set cdr))

;; create k-combination with repetition
(define (combine* n set)
  (combine3 n set (lambda (x) x)))
```

Credit: [Nils M Holm](https://t3x.org/) (ref: [combine.scm](https://t3x.org/s9fes/combine.scm.html))

## Usage

```scheme
(combine 2 '(a b c))
;; ==> ((a b) (a c) (b c))
(combine* 2 '(a b c))
;; ==>  ((a a) (a b) (a c) (b b) (b c) (c c))
```
