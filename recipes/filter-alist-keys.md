# Filter alist keys

## Problem

You have an association list and you want a copy with only the
specified keys.

## Solution

```Scheme
(define (match value)
  (lambda (x)
    (equal? x value)))

(define (alist->subset keys alist)
  (let iter ((alist alist) (keys keys) (result '()))
    (if (or (null? alist) (null? keys))
        (reverse result)
        (let* ((pair (car alist)) (key (car pair)))
          (if (member key keys)
              (let ((keys (remove (match keys) keys)))
                 (iter (cdr alist) keys (cons pair result)))
              (iter (cdr alist) keys result))))))
```

Credit [Jakub T. Jankiewicz](https://jcubic.pl/me)

It uses the `remove` function from Recipe #6

The code assumes that _alist_ has no duplicate keys.

## Usage

```Scheme
(define alist '((foo . 10) (bar . 20) (baz . 30) (quux . 40)))

(alist->subset '(quux foo) alist)
;; ==> ((foo . 10) (quux . 40))
```
