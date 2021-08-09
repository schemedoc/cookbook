# Test whether a given property exists in a sequence of N lists

## Problem

I have sequence of lists and I want to check if the lists have single element
that return true when using given comparator.

## Solution

```scheme
(define (exists? p . a*)
  (letrec
    ((car-of
       (lambda (a)
         (map car a)))
     (cdr-of
       (lambda (a)
         (map cdr a)))
     (any-null
       (lambda (a)
         (memq '() a)))
     (exists*
       (lambda (a*)
         (and (not (any-null a*))
              (or (apply p (car-of a*))
                  (exists* (cdr-of a*)))))))
    (exists* a*)))
```

Credit: [Nils M. Holm](http://t3x.org) (ref: [exists.scm](http://t3x.org/s9fes/exists.scm.html)

## Usage

```scheme
(exists < '(9 1) '(8 2) '(7 3))
;; ==>  #t because (< 1 2 3)
```
