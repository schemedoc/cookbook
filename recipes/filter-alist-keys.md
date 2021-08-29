# Filter alist keys

## Problem

You have an association list and you want a copy with only the
specified keys.

## Solution

### Assuming no duplicate keys

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

Credit: [Jakub T. Jankiewicz](https://jcubic.pl/me)

It uses the `remove` procedure from [another
recipe](../remove-element-from-list/).

### Handling duplicate keys

```Scheme
(define (alist->subset keys alist)
  (let loop ((alist alist) (new-alist '()))
    (if (null? alist) (reverse new-alist)
        (loop (cdr alist)
              (let ((pair (car alist)))
                (if (member (car pair) keys)
                    new-alist
                    (cons pair new-alist)))))))
```

Credit: [Lassi Kortela](https://github.com/lassik)

## Usage

```Scheme
(define alist '((foo . 10) (bar . 20) (baz . 30) (quux . 40)))

(alist->subset '(quux foo) alist)
;; ==> ((foo . 10) (quux . 40))
```
