# Create McCarthy's AMB operator.

## Problem

I want to implement amb operator, so I can use nondeterminism in my code.

## Solution

This solution use code from:
* [/use-list-as-stack/](Use list as Stack) - `push!` and `pop!`
* [/find-if-element-in-list-exists](Find if element exists in list) - `exists?`

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
     (letrec-syntax ((unfold-alternatives (syntax-rules ::: ()
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

Code requires syntax-rules to support [SRFI-46](https://srfi.schemers.org/srfi-46/srfi-46.html) required by R7RS.

Credit: [Jakub T. Jankiewicz](https://jcubic.pl/me)

based on code by [Nils M Holm](https://t3x.org/) (ref: [amb.scm](http://t3x.org/s9fes/amb.scm.html))

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
