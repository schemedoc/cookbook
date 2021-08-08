# Use list as queue

## Problem

I want to implement queue data structure. That will insert elements and get
them in the same order. First In First out (FIFO)
generate k-combinations from a given list.

## Solution

```scheme
(define (make-queue)
  (cons '() '()))

(define (queue! q x)
  (let ((b (list x)))
    (if (null? (car q))
        (set-cdr! q b)
        (set-cdr! (car q) b))
    (set-car! q b)))

(define (queue-empty? q)
  (null? (car q)))

(define (unqueue! q)
  (let ((x (cadr q)))
    (if (null? (cddr q))
        (set-car! q '()))
    (set-cdr! q (cddr q))
    x))

(define (unqueue* q)
  (let ((x (unqueue! q)))
    (list x q)))
```

Credit: [Nils M Holm](http://t3x.org/) (ref: [queue.scm](http://t3x.org/s9fes/queue.scm.html))

## Usage

```scheme
(let ((q (make-queue)))
  (queue! q 'foo)
  (queue! q 'bar)
  (queue! q 'baz)
  (display (unqueue! q))
  (newline)
  (display (unqueue! q))
  (newline)
  (display (unqueue! q))
  (newline)
  (display (queue-empty? q))
  (newline))
;; ==> foo
;; ==> bar
;; ==> baz
;; ==> #f
```
