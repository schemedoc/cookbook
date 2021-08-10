# Nondeterminism using amb

## Problem

I want to use McCarthy's `amb` operator to express nondeterminism in
my code.

## Solution

This solution depends on the `syntax-rules` ellipsis extension that is
present in R7RS and
[SRFI 46](https://srfi.schemers.org/srfi-46/srfi-46.html).

It also depends on two other recipes:

* [Use list as stack](/use-list-as-stack/) - `push!` and `pop!`
* [Find stride across lists](/find-stride-across-lists) - `find-stride`

```scheme
(define *amb-stack* '())

(define *amb-done* (list 'amb-done))

(define (amb-reset)
  (set! *amb-stack* '()))

(define (amb-done? x)
  (eq? *amb-done* x))

(define-syntax amb
  (syntax-rules ()
    ((_ expr ...)
     (letrec-syntax
         ((unfold-alternatives
           (syntax-rules ::: ()
             ((_)
              (begin
                (pop! *amb-stack*)
                (if (null? *amb-stack*)
                    *amb-done*
                    ((car *amb-stack*) *amb-done*))))
             ((_ a b :::)
              (let ((x (call/cc
                        (lambda (k)
                          (set-car! *amb-stack* k)
                          a))))
                (if (amb-done? x)
                    (unfold-alternatives b :::)
                    x))))))
       (begin
         (push! *amb-stack* #f)
         (unfold-alternatives expr ...))))))

(define (amb-collector)
  (let ((values '()))
    (lambda (p . v*)
      (if (exists amb-done? v*)
          values
          (begin
            (if (apply p v*)
                (push! values v*))
            (amb))))))
```

Credit: [Jakub T. Jankiewicz](https://jcubic.pl/me)

based on code by [Nils M Holm](https://t3x.org/)
(ref: [amb.scm](https://t3x.org/s9fes/amb.scm.html))

## Usage

```scheme
(begin
  (amb-reset)
  (let ((collect (amb-collector)))
    (let ((x (amb 4 1 7)))
      (let ((y (amb 6 8 2)))
        (let ((z (amb 5 3 9)))
          (display (collect > x y z))
          (newline))))))
;; ==> ((7 6 3) (7 6 5))
```
