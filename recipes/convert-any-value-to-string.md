## Problem
You need a function that will convert any value to a string

## Solution
```Scheme
(define-values (displayed written)
  (let ((repr (lambda (fn)
                (lambda (object)
                  (call-with-port (open-output-string)
                                  (lambda (port)
                                    (fn object port)
                                    (get-output-string port)))))))
    (values (repr display) (repr write))))
```
Credit [Jakub T. Jankiewicz](https://jcubic.pl/me)

### SRFI
The functions `displayed` and `written` are also defined by SRFI 166.

## Usage
```Scheme
(define (print x)
  (display x)
  (newline))

(print (written #\x))
;; ==> #\x
(print (written "foo"))
;; ==> "foo"
(print (written '(1 2 3)))
;; ==> (1 2 3)
```
