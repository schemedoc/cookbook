# Bind optional arguments

## Problem

You have a list, example from a function, and you want to simplify checking optional parameters with default values.

## Solution

```scheme
(define-syntax let-optionals
  (syntax-rules ()
    ((_ expr ((v d) ... . tail) . body)
     ($let-optionals (v ...) () (d ...) () f tail expr body))))

(define-syntax $let-optionals
  (syntax-rules ()

    ((_ () (vt ...) _ (cl ...) f tail expr body)
     (letrec ((f (case-lambda cl ... ((vt ... . tail) . body))))
       (apply f expr)))

    ((_ (vrf . vr*) (vt ...) (df . dr*) (cl ...) f . tailexprbody)
     ($let-optionals vr* (vt ... vrf) dr* (cl ... ((vt ...) (f vt ... df))) f . tailexprbody))))
```

Credit [@tallflier](https://www.reddit.com/user/tallflier/)

**NOTE**: this macro is requirement for base implementation of [SRFI-1](https://github.com/scheme-requests-for-implementation/srfi-1).

## Usage

```scheme
(define (calc num . rest)
  (let-optionals rest ((multiplier 1) (factor 10))
    (/ (* num multiplier) factor)))

(calc 10)
;; ==> 1
(calc 10 2)
;; ==> 2
(calc 10 2 5)
;; ==> 4
```
