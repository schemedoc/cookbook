# Define functors

## Problem

You want to define functors in Scheme.

A _functor_ is an abstract interface that is implemented differently
for different data types. Functors feature in the module systems of
Standard ML and OCaml, and are similar to generics in C++.

## Solution

### Using portable macros

```Scheme
;; Create an anonymous functor.
(define-syntax functor
  (syntax-rules (=> <=)
    ((functor (import ...) => (export ...) body1 body2 ...)
     (lambda (k import ...)
       body1 body2 ...
       (k export ...)))
    ((functor (export ...) <= (import ...) body1 body2 ...)
     (functor (import ...) => (export ...) body1 body2 ...))))

;; Convenience for defining functors.
(define-syntax define-functor
  (syntax-rules (=> <=)
    ((define-functor (name import ...) => (export ...)
       body1 body2 ...)
     (define name (functor (import ...) => (export ...)
                    body1 body2 ...)))
    ((define-functor (export ...) <= (name import ...)
       body1 body2 ...)
     (define-functor (name import ...) => (export ...)
       body1 body2 ...))))

;; Evaluate code within a functor and given arguments.
(define-syntax within-functor
  (syntax-rules (=> <=)
    ((within-functor (functor arg ...) => (import ...)
       body1 body2 ...)
     (functor (lambda (import ...) body1 body2 ...)
              arg ...))
    ((within-functor (import ...) <= (functor arg ...)
       body1 body2 ...)
     (within-functor (functor arg ...) => (import ...)
       body1 body2 ...))))

;; Instantiate a functor: define all of the names it exports at the top level.
(define-syntax instantiate-functor
  (syntax-rules (=> <=)
    ((instantiate-functor (functor arg ...) => (import ...))
     (define-values (import ...)
       (functor values arg ...)))
    ((instantiate-functor (import ...) <= (functor arg ...))
     (instantiate-functor (functor arg ...) => (import ...)))))
```

Credit: David Rush and [Taylor
Campbell](https://mumble.net/~campbell/)

### Using native modules

The Chicken Scheme module system has a functors facility.

## Usage

### Factorial

`numeric-fact` computes factorials with their numeric definition, but
`pair-fact` takes a list of _n_ elements and returns a list of _n_!
elements. The code for `numeric-fact` and `pair-fact` is the same.

```Scheme
(define-functor (fact-functor < two one * sub1) => (fact)
  (define (fact n)
    (if (< n two)
        one
        (* n (fact (sub1 n))))))

(instantiate-functor (numeric-fact) <=
  (fact-functor < 2 1 * (lambda (x) (- x 1))))

(instantiate-functor (pair-fact) <=
  (fact-functor (lambda (l r)         ; <
                  (let loop ((l l) (r r))
                    (cond ((null? l) (not (null? r)))
                          ((null? r) #f)
                          (else      (loop (cdr l) (cdr r))))))
                '(a a)                ; two
                '(a)                  ; one
                (lambda (x y)         ; *
                  (let loop ((r '()) (y y))
                    (if (null? y)
                        r
                        (loop (append x r) (cdr y)))))
                cdr))                 ; sub1
```

### Fibonacci

Another example of the same idea.

```Scheme
(define-functor (fib-functor <= zero one two + sub1) => (fib)
  (define (fib n)
    (cond ((<= n one) zero)
          ((<= n two) one)
          (else       (+ (fib (sub1 n)) (fib (sub1 (sub1 n))))))))

(instantiate-functor (numeric-fib) <=
  (fib-functor <= 0 1 2 + (lambda (x) (- x 1))))

(instantiate-functor (pair-fib) <=
  (fib-functor (lambda (l r)          ; our listic <= comparison operator.
                 (let loop ((l l) (r r))
                   (cond ((null? l) #t)
                         ((null? r) #f)
                         (else      (loop (cdr l) (cdr r))))))
               '()                    ; zero
               '(a)                   ; one
               '(a a)                 ; two
               append                 ; +
               cdr))                  ; sub1
```
