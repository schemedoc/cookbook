## Problem

I want to split a list into lists by predicate, the function accept two sequential elements of the list.

## Solution
    
```scheme
(define (collect p a)
  (let collect ((in  a)
                (out '())
                (res '()))
    (cond ((null? in)
            (reverse! res))
          ((and (pair? (cdr in))
                (p (car in) (cadr in)))
            (collect (cdr in)
                     (cons (car in) out)
                     res))
          (else
            (let ((out (reverse! (cons (car in) out))))
              (collect (cdr in)
                       '()
                       (cons out res)))))))
```

Credit: [Nils M Holm](http://t3x.org/)

## Usage

```scheme
(collect eq? '(a a a b c c))
;; ==> ((a a a) (b) (c c))
(collect < '(1 2 3 3 4 5 4))
;; ==> ((1 2 3) (3 4 5) (4))
```