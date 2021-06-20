## Problem
You have Alist and you need to return the new Alist only with specified keys.

## Solution
```scheme
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
Credit @jcubic

It uses the `remove` function from Recipe #6

## Usage
```scheme
(define alist '((foo . 10) (bar . 20) (baz . 30) (quux . 40)))
(alist->subset '(quux foo) alist)
;; => '((foo . 10) (quux . 40))
```