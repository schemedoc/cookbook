# Join list of strings with delimiter

## Problem
You have a list of strings and string delimiter and you want to create a single string from the list with a delimiter in between

## Solution
```scheme
(define (string-join list delimiter)
  (let iter ((list list) (result '()))
    (if (null? list)
        (apply string-append (reverse result))
        (let ((item (car list))
              (rest (cdr list)))
          (if (null? result)
              (iter rest (cons item result))
              (iter rest (cons item (cons delimiter result))))))))
```

Credit [Jakub T. Jankiewicz](https://jcubic.pl/me)

### Alternative
Alternative using string ports
```scheme
(define (string-join list delimiter)
  (if (null? list) ""
      (call-with-port (open-output-string)
                      (lambda (out)
                        (write-string (car list) out)
                        (for-each (lambda (item)
                                    (write-string delimiter out)
                                    (write-string item out))
                                  (cdr list))
                        (get-output-string out)))))
```
Credit [Lassi Kortela](https://github.com/lassik)

### SRFI
you can use SRFI-13 that provides `string-join` function.

## Usage
```scheme
(string-join '("foo" "bar" "baz") ":")
;; ==> "foo:bar:baz"
```
