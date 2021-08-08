## Problem

I want to use list as stack, to add and remove items from list.

## Solution

```scheme
(define-syntax push!
  (syntax-rules ()
    ((_ var obj)
     (set! var (cons obj var)))))

(define-syntax pop!
  (syntax-rules ()
    ((_ var)
     (let ((top (car var)))
       (set! var (cdr var))
       top))))
```

Credit: [Jakub T. Jankiewicz](https://jcubic.pl/me) based on code by [Nils M Holm](http://t3x.org/)

## Usage

```scheme
(let ((x ()))
  (push! x 'foo)
  (push! x 'bar)
  (push! x 'baz)
  (display x)
  (newline)
  (display (pop! x))
  (newline)
  (display x)
  (newline))
;; ==> (baz bar foo)
;; ==> baz
;; ==> (bar foo)
```